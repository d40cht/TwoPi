package org.seacourt.osm.route

import org.seacourt.osm._
import org.seacourt.osm.poi.POIBuilder

import scala.collection.{mutable, immutable}

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{ Input, Output }
import com.twitter.chill.{KryoSerializer}
 
object RoutableGraphBuilder extends Logging
{
    import java.io._
    import java.util.zip._
    
    
    val maxDistForLocale = 200.0 // metres
    
    // Serializes the route nodes without serializing the destination edges
    // in order to prevent Kryo stack-overflow on deep graphs.
    class RouteNodeSerializer extends Serializer[RouteNode]
    {
        def write( kryo : Kryo, output : Output, rg : RouteNode ) =
        {
            kryo.writeObject( output, rg.nodeId )
            kryo.writeObject( output, rg.node )
        }
        
        def read( kryo : Kryo, input : Input, ct : java.lang.Class[RouteNode] ) : RouteNode =
        {
            new RouteNode(
                kryo.readObject( input, classOf[Int] ),
                kryo.readObject( input, classOf[Node] ) )
        }
    }

    class RoutableGraphSerializer extends Serializer[RoutableGraph]
    {   
        def write( kryo : Kryo, output : Output, rg : RoutableGraph ) =
        {
            kryo.writeObject( output, rg.nodes )
            kryo.writeObject( output, rg.scenicPoints )
            
            for ( n <- rg.nodes )
            {
                val sinkNodeIds = n.destinations.map( _.node.nodeId ).toArray
                val edges = n.destinations.map( _.edge ).toArray
                
                kryo.writeObject( output, sinkNodeIds )
                kryo.writeObject( output, edges )
            }
        }
        
        def read( kryo : Kryo, input : Input, ct : java.lang.Class[RoutableGraph] ) : RoutableGraph =
        {
            val nodes = kryo.readObject( input, classOf[Array[RouteNode]] )
            val scenicPoints = kryo.readObject( input, classOf[Array[ScenicPoint]] )
            
            val nodeByIdMap = nodes.map( n => (n.nodeId, n) ).toMap
            for ( n <- nodes )
            {
                val sinkNodeIds = kryo.readObject( input, classOf[Array[Int]] )
                val edges = kryo.readObject( input, classOf[Array[RouteEdge]] )
                
                for ( (snid, edge) <- sinkNodeIds.zip(edges) )
                {
                    n.addEdge( nodeByIdMap(snid), edge )
                }
            }
            
            new RoutableGraph( nodes, scenicPoints )
        }
    }
    
    private def rgKryo() =
    {
        val kryo = KryoSerializer.registered.newKryo 
        
        kryo.register( classOf[RouteNode], new RouteNodeSerializer() )
        kryo.register( classOf[RoutableGraph], new RoutableGraphSerializer() )
        
        kryo
    }
    
    def save( rg : RoutableGraph, outputFile : File )
    {
        log.info( "Serialising route graph to: " + outputFile )
        
        val kryo = rgKryo()
        val output = new Output( new GZIPOutputStream( new FileOutputStream( outputFile ) ) )
        kryo.writeObject(output, rg)
        output.close
        
        log.info( "Complete." )
    }
    
    def load( inputFile : File ) : RoutableGraph =
    {
        log.info( "Reading route graph from disk." )
        val kryo = rgKryo()
        val input = new Input( new GZIPInputStream( new java.io.FileInputStream( inputFile ) ) )
        val map = kryo.readObject( input, classOf[RoutableGraph] )
        input.close
        
        log.info( "Complete." )
        
        map
    }
    
    
    
    
    def apply( osmMap : OSMMap, scenicMap : RTreeIndex[ScenicPoint], poiMap : RTreeIndex[POI] ) =
    {
        // Find all nodes which belong to more than one way
        val routeNodeIds =
        {
            var startEndNodes = mutable.ArrayBuffer[Int]()
            val nodeWayMembershipCount = mutable.Map[Int, Int]()
            
            def incCount( nid : Int ) =
            {
                nodeWayMembershipCount.update(nid, nodeWayMembershipCount.getOrElse(nid, 0) + 1)
            }
            
            for ( w <- osmMap.ways )
            {
                val nodes = w.nodeIds
                startEndNodes.append( nodes.head )
                startEndNodes.append( nodes.last )
                nodes.foreach( nid => incCount(nid) )
            }
            
            log.info( "Num nodes: %d".format( nodeWayMembershipCount.size ) )
            
            val junctionNodes = nodeWayMembershipCount
                .filter { case (nid, count) => count > 1 }
                .map { case (nid, count) => nid }
                .toSet
                
            log.info( "Num junction nodes: %d and start/end nodes: %d".format( junctionNodes.size, startEndNodes.size ) )
                
            (junctionNodes ++ startEndNodes).toSet
        }
        
        // Build the route graph
        var allSPs = mutable.HashSet[ScenicPoint]()
        
        val routeNodeMap = mutable.Map[Int, RouteNode]()
        
        val numWays = osmMap.ways.size
        var nextEdgeId = 0
        for ( (w, i) <- osmMap.ways.zipWithIndex )
        {
            if ( (i % 10000) == 0 ) log.info( "Processing way %d of %d".format( i, numWays ) )
            
            val tagMap = w.tags.map( t => (t.key, t.value) ).toMap
            
            
            val name = RouteEdge.name( tagMap )
            
            var dist = 0.0
            var absHeightDelta = 0.0
            var lastNode : Option[Node] = None
            var lastRouteNode : Option[RouteNode] = None
            var nodes = mutable.ArrayBuffer[Node]()
            var scenicPoints = mutable.ArrayBuffer[ScenicPoint]()
            var pois = Map[POI, Float]()
            for ( nid <- w.nodeIds )
            {
                val isRouteNode = routeNodeIds contains nid
                val node = osmMap.nodes(nid)
                
                val nearestScenicPoint = scenicMap.nearest(node.coord).get
                nodes.append( node )
                
                if ( nearestScenicPoint.coord.distFrom(node.coord) < maxDistForLocale )
                {
                    scenicPoints.append( nearestScenicPoint )
                    allSPs.add( nearestScenicPoint )
                }
                
                val nearestPOIs = poiMap.nearest(node.coord, 10)
                for ( nearestPOI <- nearestPOIs )
                {
                    val poiDist = nearestPOI.coord.distFrom(node.coord).toFloat
                    if ( poiDist < maxDistForLocale )
                    {
                        if ( !pois.contains( nearestPOI ) || pois(nearestPOI) > poiDist )
                        {
                            pois += (nearestPOI -> poiDist)
                        }
                    }
                }
                
                // Update cumulative way distance
                lastNode.foreach
                { ln =>
                
                    dist += ln.coord.distFrom( node.coord )
                    
                    val prevHeight = node.height
                    val thisHeight = ln.height
                    
                    absHeightDelta += prevHeight - thisHeight
                }
                
                
                if ( isRouteNode )
                {    
                    val rn = routeNodeMap.getOrElseUpdate( nid, new RouteNode(nid, node) )
                    
                    lastRouteNode.foreach
                    { lrn =>
                    
                        // If oneway=yes/true/1. forward oneway. If oneway=-1/reverse, backward oneway
                        
                    	val (forward, backward) = tagMap.get("highway") match
                    	{
                    	    case Some("yes") | Some("true") | Some("1")	=> (true, false)
                    	    case Some("reverse") | Some("-1")			=> (false, true)
                    	    case _										=> (true, true)
                    	}
                        
                        val edge = new RouteEdge(
                            wayTags = tagMap,
                            edgeId = nextEdgeId,
                            dist = dist,
                            absHeightDelta = absHeightDelta,
                            name = name,
                            scenicPoints = scenicPoints.distinct.toArray,
                            pois = pois.map { case (poi, dist) => NearbyPOI( dist, poi ) }.toArray,
                            nodes = nodes.toArray )
                          
                        // Conditionally add based on forward/backward determined above
                        rn.addEdge( lrn, edge )
                        lrn.addEdge( rn, edge )
                        nextEdgeId += 1
                    }
                    
                    lastRouteNode = Some(rn)
                    scenicPoints.clear()
                    nodes.clear()
                    nodes.append( node )
                    pois = Map()
                    dist = 0.0
                    absHeightDelta = 0.0
                }
                
                lastNode = Some(node)
            }
        }
        
        log.info( "Number of osm nodes: %d, number of route nodes: %d and edges: %d".format( osmMap.nodes.size, routeNodeMap.size, nextEdgeId ) )
        
        new RoutableGraph( routeNodeMap.map { _._2 }.toArray, allSPs.toArray )
    }
}


object GenerateRouteGraph extends App with Logging
{

    private def buildScenicMap() =
    {
        val scenicMap = new RTreeIndex[ScenicPoint]()
        
        val scenicOrNotFile = new java.io.File("data/scenicOrNot.tsv")
        val scenicScores = io.Source.fromFile( scenicOrNotFile ).getLines.drop(1).map
        { l =>
        
            val els = l.split("\t").map( _.trim)
            val lat = els(1).toDouble
            val lon = els(2).toDouble
            val score = (els(3).toDouble - 1.0) / 9.0
            val picLink = els(6)
            val imageId = picLink.split("/").last.toInt
            
            (imageId, score)
        }.toMap
        
        val simpleDateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")
        val imageDetailFile = new java.io.File("data/all_voted_dump.tsv")
        val imageMetaData = io.Source.fromFile( imageDetailFile, "iso-8859-1" ).getLines.drop(1).foreach
        { l =>
            val els = l.split("\t").map( _.trim )
            
            val imageId = els(0).toInt
            val lat = els(1).toDouble
            val lon = els(2).toDouble
            val title = els(4)
            val photographer = els(5)
            val hash = els(7)
            // TODO: Re-instate date parsing? Ask Barry to re-do the dump with this column
            // or alternatively parse it out of gridimage_base.tsv
            val dateTaken = simpleDateFormat.parse("2013-01-01")//els(5)
            val thumbs = els(8)
            val galleryAvg = els(9)
            
            val score = scenicScores.get( imageId ) match
            {
                case Some(s)	=> s
                case None		=>
                {
                    // This is really rather crude as the relation between gallery scores and scenicornot may well
                    // not be linear
                	if ( galleryAvg != "NULL" && galleryAvg != "0" ) (galleryAvg.toDouble - 1.0) / 4.0
                	// OK - all we have is a number of thumbs. But no idea of number of views. Score of 0.5
                	else 0.5
                }
            }
            
            val c = new Coord( lat=lat, lon=lon )
            scenicMap.add( c, new ScenicPoint( c, score.toFloat, photographer, title, imageId, hash ) )
        }
        
        scenicMap
    }

    override def main( args : Array[String] )
    {
        val mapFile = new java.io.File( args(0) )
        
        val map = OSMMap.load( mapFile )
        
        log.info( "Building scenic map" )
        val scenicMap = buildScenicMap()
        
        
        log.info( "Building wikipedia cross-linked POIs" )
        val poiMap = new RTreeIndex[POI]()
        POIBuilder.build(map).foreach { poi => poiMap.add( poi.coord, poi ) }
        
        val rgFile = new java.io.File(mapFile + ".rg")
        log.info( "Building RoutableGraph" )
        val rgi = RoutableGraphBuilder( map, scenicMap, poiMap )
        
        log.info( "Saving graph to: " + rgFile.toString )
        RoutableGraphBuilder.save( rgi, rgFile )
        
        
        log.info( "Loading graph to check serialization logic")
        val rg = RoutableGraphBuilder.load( rgFile )
        
        log.info( "Complete..." )
    }
}

