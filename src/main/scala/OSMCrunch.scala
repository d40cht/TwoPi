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

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{ Input, Output }

import com.twitter.chill._

trait Logging
{
    lazy val log = Logger.get(getClass)
}

case class Tag( val keyId : Int, val valueId : Int )
{
    def this() = this(-1, -1)
}



class StringMap
{
    private var nextId = 0
    private val stringMap = mutable.Map[String, Int]()
    private val stringArray = mutable.ArrayBuffer[String]()
    
    def apply( s : String ) : Int =
    {
        stringMap.get(s) match
        {
            case Some(id)   => id
            case _          =>
            {
                val sId = nextId
                val cs = new String(s)
                stringArray.append( cs )
                stringMap.put( cs, sId )
                nextId += 1
                sId
            }
        }
    }
    
    def apply( id : Int ) = stringArray(id)
}

case class TagStringRegistry( val keyMap : StringMap, val valMap : StringMap )
{   
    def this() = this( new StringMap(), new StringMap() )
    def apply( key : String, value : String ) = new Tag( keyMap(key), valMap(value) )
}


case class Coord( val lon : Double, val lat : Double )
{
    def this() = this(0.0, 0.0)
}
case class Node( val coord : Coord, val tags : Array[Tag] )
{
    def this() = this( null, Array() )
}
case class Way( val id : Long, val nodeIds : Array[Int], val tags : Array[Tag] )
{
    def this() = this( -1, Array(), Array() )
}

case class OSMMap( val nodes : Array[Node], val ways : Array[Way], val tagRegistry : TagStringRegistry )
{
    def this() = this( Array(), Array(), null )
}

object OSMMap extends Logging
{
    def save( map : OSMMap, fileName : File )
    {
        import java.io._
        import java.util.zip._
        
        log.info( "Serialising to: " + fileName )
        val kryo = new Kryo()
        
        val output = new Output( new GZIPOutputStream( new FileOutputStream( fileName ) ) )
        kryo.writeObject(output, map)
        output.close
        
        log.info( "Complete." )
    }
    
    def load( fileName : File ) : OSMMap =
    {
        log.info( "Reading map from disk." )
        val kryo = new Kryo()
        val input = new Input( new GZIPInputStream( new java.io.FileInputStream( fileName ) ) )
        val map = kryo.readObject( input, classOf[OSMMap] )
        log.info( "Number of ways: " + map.ways.size )
        
        map
    }
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

class HighwayNodes extends SimpleSink
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
                
                // Tag: highway=* or junction=*
                if ( ((wayTags contains "highway") || (wayTags contains "junction")) )
                {
                    val nodeIds = w.getWayNodes().map( _.getNodeId() ).toArray
                    nodeIds.foreach( nid => wayNodeSet.add(nid) )
                }
            }
            
            case _ =>
        }
    }
}

class CrunchSink( val wayNodeSet : mutable.Set[Long] ) extends SimpleSink
{
    import scala.collection.JavaConversions._
    
    def inUk( c : Coord ) = c.lon > -9.23 && c.lon < 2.69 && c.lat > 49.84 && c.lat < 60.85
        
    var ukNodes = 0
    var ukWays = 0
    var ukWayNodes = 0
    
    val tsr = new TagStringRegistry()
    
    val nodesById = mutable.Map[Long, Int]()
    val ways = mutable.ArrayBuffer[Way]()
    val nodes = mutable.ArrayBuffer[Node]()
    
    
    def process(entityContainer : EntityContainer)
    {
        val entity = entityContainer.getEntity()
        
        entity match
        {
            case n : v0_6.Node =>
            {
                val c = new Coord( n.getLongitude(), n.getLatitude() )
                val nId = n.getId()
                
                if ( wayNodeSet contains nId )
                {
                    ukNodes += 1
                    if ( (ukNodes % 100000) == 0 ) log.info( "Nodes: " + ukNodes.toDouble / 1000000.0 + "M" )
                    
                    val nodeTags = n.getTags().map { t => tsr( t.getKey(), t.getValue() ) }.toArray
                    
                    nodes.append( Node( c, nodeTags ) )
                    nodesById.put( nId, nodes.size-1 )
                }
            }
            
            case w : v0_6.Way =>
            {
                val nodeIds = w.getWayNodes().map( _.getNodeId() ).toArray
                
                val haveAllNodes = nodeIds.forall( nid => wayNodeSet contains nid )
                val wayTags = w.getTags().map { t => ( t.getKey(), t.getValue() ) }.toMap
                
                // Tag: highway=* or junction=*
                if ( haveAllNodes && ((wayTags contains "highway") || (wayTags contains "junction")) )
                {
                    val wayNodes = nodeIds.map( nid => nodesById(nid) )
                    ukWays += 1
                    ukWayNodes += nodeIds.length
                    if ( (ukWays % 10000) == 0 ) log.info( "Ways: " + ukWays.toDouble / 1000000.0 + "M" + ", " + ukWayNodes.toDouble / 1000000.0 + "M" )
                    val way = Way( w.getId(), wayNodes, wayTags.toArray.map( t => tsr(t._1, t._2) ) )
                    ways.append(way)
                    
                    wayNodes.foreach( wnid => wayNodeSet.add(wnid) )
                }
            }
            
            // TODO: And relations please
            
            case _ =>
        }
    }
    
    def getData() =
    {
        new OSMMap( nodes.toArray, ways.toArray, tsr )
    }
}

class OSMCrunch( val dataFileName : File ) extends Logging
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
                val hn = new HighwayNodes()
                reader.setSink( hn )   
                reader.run()
                
                hn.wayNodeSet
            }
            
            log.info( "Pass 2: Building map" )
            val reader = new OsmosisReader( new BufferedInputStream( new FileInputStream( dataFileName ) ) )
            val cs = new CrunchSink(wayNodeSet)
            reader.setSink( cs )   
            reader.run()
            cs.getData()
        }
        
        osmMap
        
    }
}

object OSMCrunch extends App
{    
    override def main( args : Array[String] )
    {
        Logger.clearHandlers()
        LoggerFactory( node="org.seacourt", handlers = List(ConsoleHandler( level = Some( Level.INFO ) )) ).apply()

        val mapFile = new File( args(1) )
        
        { 
            val map =
            {
                val osmc = new OSMCrunch( new File(args(0)) )
                osmc.run()
            }
            
            OSMMap.save( map, mapFile )
        }
        
        val loadedMap = OSMMap.load( mapFile )
    }
}

// Yuk yuk yuk.
class PackedIntPairArray
{
    private val cont = mutable.ArrayBuffer[Long]()
    
    def add( a : Int, b : Int ) { cont.append( a.toLong << 32 | b.toLong ) }
    def apply( index : Int ) : (Int, Int) =
    {   
        val res = cont(index)
        
        ((res >> 32).toInt, res.toInt)
        
    }
    
    def size = cont.size
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
    
    implicit class RichTag( val tag : Tag )
    {
        def key = map.tagRegistry.keyMap( tag.keyId )
        def value = map.tagRegistry.valMap( tag.valueId )
    }
}

object CalculateWayLength extends App with Logging
{
    import com.vividsolutions.jts.geom.{Coordinate, Envelope}
    
    def distFrom(lat1 : Double, lng1 : Double, lat2 : Double, lng2 : Double ) : Double =
    {
        val earthRadius = 3958.75;
        val dLat = Math.toRadians(lat2-lat1)
        val dLng = Math.toRadians(lng2-lng1)
        val a = Math.sin(dLat/2) * Math.sin(dLat/2) +
                   Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
                   Math.sin(dLng/2) * Math.sin(dLng/2)
        val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a))
        val dist = earthRadius * c

        val meterConversion = 1609

        dist * meterConversion
    }

    
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
                acc += distFrom( n1.coord.lat, n1.coord.lon, n2.coord.lat, n2.coord.lon )
                segments +=1
                
            }
        }
        
        log.info( "Total distance: " + acc + ", average segment length: " + (acc/segments.toDouble) )
    }
}

