package org.seacourt.osm


import java.io.{File, BufferedInputStream, FileInputStream, FileOutputStream}
import java.util.zip._

import crosby.binary.osmosis.OsmosisReader

import org.openstreetmap.osmosis.core.task.v0_6.Sink
import org.openstreetmap.osmosis.core.container.v0_6.EntityContainer
import org.openstreetmap.osmosis.core.domain.v0_6

import scala.collection.{mutable, immutable}

import com.twitter.logging.Logger
import com.twitter.logging.{Logger, LoggerFactory, FileHandler, ConsoleHandler, Policy}
import com.twitter.logging.config._


/* TODO:

    * Re-code gpx file to binary (using Kryo streams)
    * Experiment (with particle filters) snapping gpx traces to the map
    * Experiment with auto-classifying gpx files
*/

trait Logging
{
    lazy val log = Logger.get(getClass)
}

object Logging
{
    import com.twitter.logging.{Logger, LoggerFactory, FileHandler, ConsoleHandler, Policy}
    import com.twitter.logging.config._
    import java.io._
    
    val TRACE	= Level.TRACE
    val DEBUG	= Level.DEBUG
    val INFO	= Level.INFO
    val WARN	= Level.WARNING
    val ERROR	= Level.ERROR
    
    def configureDefaultLogging( logFileName : Option[File] = None, level : Level = DEBUG )
    {
        val handlers = logFileName match
        {
            case None => List( ConsoleHandler( level = Some( Level.INFO ) ) )
            case Some( f ) => List(
                FileHandler(
                    filename = f.getAbsolutePath,
                    append = false,
                    level = Some(level),
                    rollPolicy = Policy.SigHup,
                    rotateCount=8
                ),
                    ConsoleHandler( level = Some( Level.INFO ) )
            )
        }
                
        Logger.clearHandlers()
        LoggerFactory(
            node = "org.seacourt",
            level = Some(level),
            handlers = handlers
        ).apply()
    }
}

case class Tag( val key : String, val value : String )
{
}



object Coord
{
    val earthRadius = 6371.0 // KM
}

case class Coord( val lon : Double, val lat : Double )
{   
    def interpolate( other : Coord, frac : Double ) =
    {
        assert( frac >= 0.0 && frac <= 1.0, "Frac outside expected range: " + frac )
        
        new Coord( other.lon * frac + lon * (1.0-frac), other.lat * frac + lat * (1.0-frac) )
    }
    
    // Returns metres
    def distFrom( other : Coord ) : Double = 
    {
        val (lat1, lng1) = (lat, lon)
        val (lat2, lng2) = (other.lat, other.lon)
        
        val dLat = Math.toRadians(lat2-lat1)
        val dLng = Math.toRadians(lng2-lng1)
        val a = Math.sin(dLat/2) * Math.sin(dLat/2) +
                Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
                Math.sin(dLng/2) * Math.sin(dLng/2)
        val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a))
        val dist = Coord.earthRadius * c

        dist * 1000.0
    }
    
    // Bearing is in degrees, +/- 180.0
    def bearing( other : Coord ) : Double =
    {
        val (lat1, lng1) = (lat, lon)
        val (lat2, lng2) = (other.lat, other.lon)
        
        val dLat = Math.toRadians(lat2-lat1)
        val dLng = Math.toRadians(lng2-lng1)
        
        val y = Math.sin(dLng) * Math.cos(Math.toRadians(lat2))
        val x = Math.cos(Math.toRadians(lat1)) * Math.sin(Math.toRadians(lat2)) -
                Math.sin(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.cos(dLng)
                
        val bearing = Math.toDegrees( Math.atan2(y, x) )
        
        assert( bearing >= -180.0 && bearing <= 180.0 )
        
        bearing
    }
}

case class Node( val coord : Coord, val height : Float, val tags : Array[Tag] )
{
    def tagMap = tags.map( t => (t.key, t.value) ).toMap
}

case class Way( val nodeIds : Array[Int], val tags : Array[Tag] )
{
}

case class OSMMap( val nodes : Array[Node], val ways : Array[Way], val poiNodes : Array[Node] )
{
}

object OSMMap extends Logging
{
    def save( map : OSMMap, fileName : File ) = Utility.kryoSave( map, fileName )
    def load( fileName : File ) : OSMMap = Utility.kryoLoad[OSMMap]( fileName )
}

abstract class SimpleSink extends Sink with Logging
{
    def initialize(metaData : java.util.Map[String, Object])
    {
        log.debug( "initialize" )
    }
    
    def complete()
    {
        log.debug( "complete" )
    }
    
    def release()
    {
        log.debug( "release" )
    }
}

object NodePredicates
{
    // Note that some ways can have such settings - e.g. amenity cafe for a delineated building.
    private val nodesOfInterest = Map[String, String => Boolean](
        "amenity"   -> (v => Set("pub", "cafe", "drinking_water", "parking", "fuel"/*, "fast_food", "food_court", "ice_cream", "fuel", "atm", "telephone", "toilets"*/) contains v),
        "historic"  -> (v => true),
        "tourism"   -> (v => Set("attraction", "artwork", "viewpoint", "wilderness_hut") contains v),
        "natural"   -> (v => Set("cave_entrance", "peak", "wetland", "bay", "beach", "spring", "scree", "volcano") contains v),
        "place"     -> (v => Set("village", "town", "hamlet", "isolated_dwelling", "farm", "locality") contains v)
    ) 
    
    
    def isHighway( tags : Map[String, String] ) =  ((tags contains "highway") || (tags contains "junction"))

    def isOfInterest( tags : Map[String, String] ) : Boolean = //n : v0_6.Node ) : Boolean =
    {
        val isNOI = tags
            .exists
            { case (k, v) =>
                
                nodesOfInterest.get( k ) match
                {
                    case Some(fn) if fn( v ) => true
                    case _                              => false
                }
            }
            
        isNOI
    }
}

class RelevantWayNodes extends SimpleSink
{
    import scala.collection.JavaConversions._
    
    val wayNodeSet = mutable.Set[Long]()
    
    def process(entityContainer : EntityContainer)
    {
        val entity = entityContainer.getEntity()
        
        entity match
        {
            case w : v0_6.Way =>
            {
                val wayTags = w.getTags().map { t => ( t.getKey(), t.getValue() ) }.toMap
                
                if ( NodePredicates.isHighway(wayTags) || NodePredicates.isOfInterest(wayTags) )
                {
                    val nodeIds = w.getWayNodes().map( _.getNodeId() ).toArray
                    nodeIds.foreach( nid => wayNodeSet.add(nid) )
                }
            }
            
            case _ =>
        }
    }
}




class CrunchSink( val wayNodeSet : mutable.Set[Long], val heightMap : SRTMInMemoryTiles ) extends SimpleSink
{
    import scala.collection.JavaConversions._
    
    def inUk( c : Coord ) = c.lon > -9.23 && c.lon < 2.69 && c.lat > 49.84 && c.lat < 60.85
        
    var ukNodes = 0
    var ukWays = 0
    var ukWayNodes = 0
    
    val nodesById = mutable.Map[Long, Int]()
    val ways = mutable.ArrayBuffer[Way]()
    val nodes = mutable.ArrayBuffer[Node]()
    
    val poiNodes = mutable.ArrayBuffer[Node]()
    
    
    def process(entityContainer : EntityContainer)
    {
        val entity = entityContainer.getEntity()
        
        entity match
        {
            case n : v0_6.Node =>
            {
                val c = new Coord( n.getLongitude(), n.getLatitude() )
                val nId = n.getId()
                
                val nodeTags = n.getTags().map { t => (t.getKey(), t.getValue()) }.toMap
                val isPOINode = NodePredicates.isOfInterest(nodeTags)
                if ( (wayNodeSet contains nId) || isPOINode )
                {
                    ukNodes += 1
                    if ( (ukNodes % 100000) == 0 ) log.info( "Nodes: " + ukNodes.toDouble / 1000000.0 + "M" )
                    
                    nodesById.put( nId, nodes.size )
                    val theNode = Node( c, heightMap.elevation( c.lon, c.lat ).getOrElse(-9999.0).toFloat, nodeTags.map( v => Tag(v._1, v._2) ).toArray )
                    if ( isPOINode )
                    {
                        poiNodes.append( theNode )
                    }
                    nodes.append( theNode )
                }
            }
            
            case w : v0_6.Way =>
            {
                val nodeIds = w.getWayNodes().map( _.getNodeId() ).toArray
                
                val haveAllNodes = nodeIds.forall( nid => wayNodeSet contains nid )
                val wayTags = w.getTags().map { t => ( t.getKey(), t.getValue() ) }.toMap
                
                // Tag: highway=* or junction=*
                if ( haveAllNodes )
                {
                    if ( NodePredicates.isHighway(wayTags) )
                    {
                        val wayNodes = nodeIds.map( nid => nodesById(nid) )
                        ukWays += 1
                        ukWayNodes += nodeIds.length
                        if ( (ukWays % 10000) == 0 ) log.info( "Ways: " + ukWays.toDouble / 1000000.0 + "M" + ", " + ukWayNodes.toDouble / 1000000.0 + "M" )
                        val way = Way( wayNodes, wayTags.toArray.map( t => Tag(t._1, t._2) ) )
                        ways.append(way)
                        
                        wayNodes.foreach( wnid => wayNodeSet.add(wnid) )
                    }
                    else if ( NodePredicates.isOfInterest(wayTags) )
                    {
                        val wayNodes = nodeIds.map( nid => nodes(nodesById(nid)) )
                        val sumCoord = wayNodes.map( _.coord )
                            .foldLeft( Coord(0.0, 0.0) ) { case (acc, c) => Coord( acc.lon + c.lon, acc.lat + c.lat ) }
                        val meanCoord = Coord( sumCoord.lon / wayNodes.size.toDouble, sumCoord.lat / wayNodes.size.toDouble )
                        
                        poiNodes.append( Node( meanCoord, heightMap.elevation( meanCoord.lon, meanCoord.lat ).getOrElse(-9999.0).toFloat, wayTags.toArray.map( t => Tag(t._1, t._2) ) ) )

                    }
                }
            }
            
            // TODO: And relations please
            
            case _ =>
        }
    }
    
    def getData() =
    {
        new OSMMap( nodes.toArray, ways.toArray, poiNodes.toArray )
    }
}

class OSMCrunch( val dataFileName : File, val heightMap : SRTMInMemoryTiles ) extends Logging
{
    import java.io._
    
    def run() : OSMMap =
    {
        val osmMap = 
        {
            log.info( "Pass 1: Collecting ids of highway nodes" )
            val wayNodeSet =
            {
                val reader = new OsmosisReader( new BufferedInputStream( new FileInputStream( dataFileName ) ) )
                val hn = new RelevantWayNodes()
                reader.setSink( hn )   
                reader.run()
                
                hn.wayNodeSet
            }
            
            log.info( "Pass 2: Building map" )
            val reader = new OsmosisReader( new BufferedInputStream( new FileInputStream( dataFileName ) ) )
            val cs = new CrunchSink( wayNodeSet, heightMap )
            reader.setSink( cs )   
            reader.run()
            cs.getData()
        }
        
        osmMap
        
    }
}

object OSMCrunch extends App with Logging
{    
    override def main( args : Array[String] )
    {
        Logger.clearHandlers()
        LoggerFactory( node="org.seacourt", handlers = List(ConsoleHandler( level = Some( Level.INFO ) )) ).apply()

        val srtmFiles = new java.io.File( "./data" ).listFiles.filter( _.toString.endsWith(".asc") )
        log.info( "Found SRTM files: " + srtmFiles.toList )
        val heightMap = new SRTMInMemoryTiles( srtmFiles )
        
        val mapFile = new File( args(1) )
        
        { 
            val map =
            {
                val osmc = new OSMCrunch( new File(args(0)), heightMap )
                osmc.run()
            }
            
            OSMMap.save( map, mapFile )
        }
        
        val loadedMap = OSMMap.load( mapFile )
    }
}


class MapWithIndex( val map : OSMMap )
{
    import com.infomatiq.jsi.{Rectangle, Point}
    import com.infomatiq.jsi.rtree.RTree
    
    private val (index, nodeToWayMap) =
    {
        var nodeToWayMap = mutable.Map[Int, Array[Int]]()
        
        val t = new RTree()
        t.init(null)

        map.ways.zipWithIndex.foreach
        { case (w, wi) =>
        
            w.nodeIds.foreach
            { nId =>
                
                val n = map.nodes(nId)
                if ( !nodeToWayMap.contains(nId) )
                {
                    t.add( new Rectangle( n.coord.lon.toFloat, n.coord.lat.toFloat, n.coord.lon.toFloat, n.coord.lat.toFloat ), nId )
                }
                val old = nodeToWayMap.getOrElse( nId, Array[Int]() )
                
                nodeToWayMap.put( nId, (old :+ wi).distinct )
            }
        }
        
        (t, nodeToWayMap)
    }
    
    def get( c : Coord, n : Int  ) : Seq[(Node, Way)] =
    {
        val nis = mutable.ArrayBuffer[Int]()
        
        index.nearestN(
            new Point( c.lon.toFloat, c.lat.toFloat ),
            new gnu.trove.TIntProcedure
            {
                def execute( ni : Int ) =
                {
                    nis.append(ni)
                    true
                }
            },
            n,
            Float.MaxValue )
            
        nis.flatMap( ni => nodeToWayMap(ni).distinct.map( wi => (map.nodes(ni), map.ways(wi)) ) )
    }
}

/*
object RecalculateAddPoints extends App with Logging
{
    def recalculate( inputMap : OSMMap, maxNodeDist : Double ) : OSMMap =
    {
        val newNodes = mutable.ArrayBuffer[Node]()
        val newWays = mutable.ArrayBuffer[Way]()
        
        def getInpNode( nId : Int ) = inputMap.nodes(nId)
        
        def addNode( n : Node ) : Int =
        {
            newNodes.append( n )
            newNodes.size - 1
        }
        
        def addWay( w : Way ) =
        {
            newWays.append( w )
        }
        
        for ( (w, i) <- inputMap.ways.zipWithIndex )
        {
            if ( (i % 1000) == 0 ) log.info( "Adding way: " + i )
          
            if ( !w.nodeIds.isEmpty )
            {
                val wayNodeIds = mutable.ArrayBuffer[Int]()
                wayNodeIds.append( addNode( getInpNode(w.nodeIds.head) ) )

                
                for ( Array(nId1, nId2) <- w.nodeIds.sliding(2) )
                {
                    val n1 = inputMap.nodes(nId1)
                    val n2 = inputMap.nodes(nId2)
                    val segmentDist = n1.coord.distFrom( n2.coord )
                    
                    if ( segmentDist > maxNodeDist )
                    {
                        val newPoints = (segmentDist / maxNodeDist).toInt
                        val increment = segmentDist / newPoints.toDouble
                        for ( i <- 1 until newPoints )
                        {
                            val frac = i.toDouble / newPoints.toDouble
                            val pc = n1.coord.interpolate( n2.coord, frac )
                            val tags = Array[Tag]()
                            wayNodeIds.append( addNode( new Node( pc, tags/*, true*/ ) ) )
                        }
                    }
                    wayNodeIds.append( addNode( n2 ) )
                }
                
                addWay( new Way( wayNodeIds.toArray, w.tags ) )
            }
        }
        
        

        new OSMMap( newNodes.toArray, newWays.toArray, inputMap.poiNodes )
    }
    
    override def main( args : Array[String] )
    {
        val loadedMap = OSMMap.load( new File( args(0) ) )
        val res = recalculate( loadedMap, 50.0 )
        OSMMap.save( res, new File( args(1) ) )
    }
}
*/

object CalculateWayLength extends App with Logging
{
    override def main( args : Array[String] )
    {
        Logger.clearHandlers()
        LoggerFactory( node="org.seacourt", handlers = List(ConsoleHandler( level = Some( Level.INFO ) )) ).apply()
        
        val loadedMap = OSMMap.load( new File( args(0) ) )
     
        var acc = 0.0
        var segments = 0

        for ( (w, i) <- loadedMap.ways.zipWithIndex )
        {
            if ( (i % 1000) == 0 ) log.info( "Adding way: " + i )
           
            for ( Array(nId1, nId2) <- w.nodeIds.sliding(2) )
            {
                val n1 = loadedMap.nodes(nId1)
                val n2 = loadedMap.nodes(nId2)
                val segmentDist = n1.coord.distFrom( n2.coord )
                acc += segmentDist
                segments +=1
                
            }
        }
        
        log.info( "Total distance: " + acc + ", average segment length: " + (acc/segments.toDouble) )
    }
}

/*

In metadata.xml, for each track:

<gpxFile id="912" timestamp="2005-11-07T16:02:38Z" points="656" lat="50.8240170" lon="-0.1451670" visibility="public" uid="999" user="mikelmaron" filename="public/000/000/000000912.gpx">
  <description>a cab ride from lewes to the nearest harley davidson outlet on bonfire night</description>
  <tags>
    <tag>lewes</tag>
  </tags>
</gpxFile>
*/


case class GPXTrackPoint( val lon : Double, val lat : Double, val time : java.util.Date )
case class GPXTrackSeg( val points : Array[GPXTrackPoint] )
case class GPXTrack( val fname : String, val name : String, val segs : Array[GPXTrackSeg] )

object ProcessGPXToBin extends App with Logging
{
    import com.esotericsoftware.kryo.{Kryo, Serializer}
    import com.esotericsoftware.kryo.io.{ Input, Output }
    import com.twitter.chill.{KryoSerializer}
    
    override def main( args : Array[String] )
    {
        import java.util.zip.{GZIPInputStream}
        import org.xeustechnologies.jtar._
        
        Logger.clearHandlers()
        LoggerFactory( node="org.seacourt", handlers = List(ConsoleHandler( level = Some( Level.INFO ) )) ).apply()
        
        // Process the metadata first
        /*val pp = new scala.xml.pull.XMLEventReader( io.Source.fromFile( new java.io.File("/home/alex/Devel/AW/tmp/gpx-planet-2013-04-09/metadata.xml") ) )
        
        pp.foreach
        {
            case s : scala.xml.pull.EvElemStart =>
            case e : scala.xml.pull.EvElemStart =>
        }*/
        
        //"/backup/Data/OSM/gpx-planet-2013-04-09.tar.gz"
        val jt = new TarInputStream( new BufferedInputStream(
            new GZIPInputStream( new FileInputStream( args(0) ) ) ) )
            
        val kryo = KryoSerializer.registered.newKryo
        val output = new Output( new GZIPOutputStream( new FileOutputStream( args(1) ) ) )
        
        //2011-07-15T10:52:03Z
        //yyyy-MM-dd'T'HH:mm:ss.SSZ
        val df = new java.text.SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" )
        var te = jt.getNextEntry()
        val toProcess = mutable.ArrayBuffer[(String, Array[Byte])]()

        while ( te != null )
        {
            val fname = new String( te.getName() )
            
            if ( fname.toString.endsWith(".gpx") )
            {
                log.info( fname )
                  
                val toAlloc = te.getSize()
                val data = new Array[Byte](toAlloc.toInt)
                val size = jt.read(data)
           
                try
                {
                    val tp = new java.io.ByteArrayInputStream(data)
                    val inXML = scala.xml.XML.load( tp )
                    tp.close
                    
                    val tracks = (inXML \\ "trk").seq.map
                    { trk =>
                    
                        val name = new String( (trk \ "name").text )
                        val segs = (trk \\ "trkseg").map
                        { seg =>
                        
                            val points = ( seg \ "trkpt" ).map
                            { trkpt =>
                            
                                val lat = (trkpt \ "@lat").text.toDouble
                                val lon = (trkpt \ "@lat").text.toDouble
                                val timeText = new String( (trkpt \ "time").text.trim )
                                
                                val time = df.parse(timeText)
                                
                                new GPXTrackPoint( lon, lat, time )
                            }.toArray
                            
                            new GPXTrackSeg( points )
                        }.toArray
                        
                        new GPXTrack( fname, name, segs )
                    }.toList
                    
                    tracks.foreach
                    { track =>
                    
                        kryo.writeObject(output, track)
                        output.flush
                    }
                }
                catch
                {
                    case e : java.lang.Throwable =>
                    {
                        log.error( "GPX trail parse failed: " + e.toString )
                    }
                }
            }
            
            te = jt.getNextEntry()
        }
        
        output.close
    }
}

