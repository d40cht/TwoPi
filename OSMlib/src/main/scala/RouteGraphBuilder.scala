package org.seacourt.osm.route

import org.seacourt.osm._
import org.seacourt.osm.poi.POIBuilder

import scala.collection.{mutable, immutable}

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{ Input, Output }
import com.twitter.chill._

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
            kryo.writeObject( output, rg.coord )
            kryo.writeObject( output, rg.height )
        }
        
        def read( kryo : Kryo, input : Input, ct : java.lang.Class[RouteNode] ) : RouteNode =
        {
            new RouteNode(
                kryo.readObject( input, classOf[Int] ),
                kryo.readObject( input, classOf[Coord] ),
                kryo.readObject( input, classOf[Float] ) )
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
        val kryo = new Kryo()
        
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
        
        log.info( "Complete." )
        
        map
    }
    
    
    def apply( osmMap : OSMMap, scenicMap : RTreeIndex[ScenicPoint], heightMap : SRTMInMemoryTiles, poiMap : RTreeIndex[POI] ) =
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
        
        {
            val routeNodeMap = mutable.Map[Int, RouteNode]()
            
            var nextEdgeId = 0
            for ( w <- osmMap.ways )
            {
                val tagMap = w.tags.map( t => (t.key, t.value) ).toMap
                
                val highwayAnnotation = tagMap.get("highway")
                val junctionAnnotation = tagMap.get("junction")
                val bridgeAnnotation = tagMap.get("bridge")
                val nameAnnotation = tagMap.get("name")
                val refAnnotation = tagMap.get("ref")
                    
                // Other important things:
                // ford: yes - In the case of Duxford Ford, this is not fordable.
                var costMultiplierOption = highwayAnnotation match
                {
                     case None => None
                     case Some( valueString ) =>
                     {
                        //if ( valueString.startsWith( "motorway" ) ) Some( 10.0 )
                        if ( valueString.startsWith( "trunk" ) ) Some( 20.0 )
                        else if ( valueString.startsWith( "primary" ) ) Some( 10.0 )
                        //else if ( valueString.startsWith( "service" ) ) 1.1
                        // Not yet classified, so be conservative
                        else if ( valueString.startsWith( "road" ) ) Some( 1.4 )
                        else if ( valueString.startsWith( "secondary" ) ) Some( 1.4 )
                        else if ( valueString.startsWith( "tertiary" ) ) Some( 1.3 )
                        else if ( valueString.startsWith( "unclassified" ) ) Some( 1.3 )
                        else if ( valueString.startsWith( "cycleway" ) ) Some( 1.2 )
                        else if ( valueString.startsWith( "residential" ) ) Some( 1.1 )
                        else if ( valueString.startsWith( "track" ) ) Some( 0.7 )
                        else if ( valueString.startsWith( "bridleway" ) ) Some( 0.6 )
                        else if ( valueString.startsWith( "footway" ) ) Some( 0.6 )
                        else if ( valueString.startsWith( "footpath" ) ) Some( 0.6 )
                        else None
                     }
                }
                
                costMultiplierOption.foreach
                { costMultiplierPre =>
                
                    val name = if ( !nameAnnotation.isEmpty ) nameAnnotation.get
                    else if ( !refAnnotation.isEmpty ) refAnnotation.get
                    else if ( !junctionAnnotation.isEmpty ) junctionAnnotation.get
                    else if ( !bridgeAnnotation.isEmpty ) "bridge"
                    else "Unnamed " + highwayAnnotation.get
                    
                    var costMultiplier = costMultiplierPre
                    refAnnotation.foreach
                    { n =>
                        if (n.matches("A[0-9]+"))
                        {
                            costMultiplier = 1.5
                        }
                    }
                    
                    
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
                        
                        val nearestPOI = poiMap.nearest(node.coord).get
                        val poiDist = nearestPOI.coord.distFrom(node.coord).toFloat
                        if ( poiDist < maxDistForLocale )
                        {
                            if ( !pois.contains( nearestPOI ) || pois(nearestPOI) > poiDist )
                            {
                                pois += (nearestPOI -> poiDist)
                            }
                        }
                        
                        // Update cumulative way distance
                        lastNode.foreach
                        { ln =>
                        
                            dist += ln.coord.distFrom( node.coord )
                            
                            val prevHeightO = heightMap.elevation( node.coord.lon, node.coord.lat )
                            val thisHeightO = heightMap.elevation( ln.coord.lon, ln.coord.lat )
                            
                            (prevHeightO, thisHeightO) match
                            {
                                case (Some(prevHeight), Some(thisHeight)) => absHeightDelta += prevHeight - thisHeight
                                case _ =>
                            }
                        }
                        
                        if ( isRouteNode )
                        {
                            val scenicScore = if ( !scenicPoints.isEmpty )
                            {
                                val scenicValue = scenicPoints.map( _.score ).sum / scenicPoints.size.toDouble
                                (1.0 + (0.5-scenicValue))
                            }
                            else
                            {
                                1.0
                            }
                            
                            val inclineScore = 1.0 - ((absHeightDelta / dist)*5.0)
                            
                            val height = heightMap.elevation( node.coord.lon, node.coord.lat ) match
                            {
                                case Some(h) => h
                                case None =>
                                {
                                    println( "No height for coords: " + node.coord.toString )
                                    -9999.0
                                }
                            }
                            val rn = routeNodeMap.getOrElseUpdate( nid, new RouteNode(nid, node.coord, height.toFloat) )
                            
                            
                            
                            lastRouteNode.foreach
                            { lrn =>
                            
                                // If oneway=yes/true/1. forward oneway. If oneway=-1/reverse, backward oneway
                                val edge = new RouteEdge(
                                    nextEdgeId,
                                    dist,
                                    dist * costMultiplier * scenicScore * inclineScore,
                                    name,
                                    scenicPoints.distinct.toArray,
                                    pois.map(_.swap).toArray, nodes.toArray )
                                    
                                rn.addEdge( lrn, edge )
                                lrn.addEdge( rn, edge )
                                nextEdgeId += 1
                            }
                            
                            lastRouteNode = Some(rn)
                            scenicPoints.clear()
                            nodes.clear()
                            pois = Map()
                            dist = 0.0
                            absHeightDelta = 0.0
                        }
                        
                        lastNode = Some(node)
                    }
                }
            }
            
            log.info( "Number of osm nodes: %d, number of route nodes: %d and edges: %d".format( osmMap.nodes.size, routeNodeMap.size, nextEdgeId ) )
            
            new RoutableGraph( routeNodeMap.map { _._2 }.toArray, allSPs.toArray )
        }
    }
}



object GenerateRouteGraph extends App with Logging
{    
    override def main( args : Array[String] )
    {
        val mapFile = new java.io.File( args(0) )
        
        val map = OSMMap.load( mapFile )
        
        val scenicMap = new RTreeIndex[ScenicPoint]()
        io.Source.fromFile( new java.io.File("data/scenicOrNot.tsv") ).getLines.drop(1).foreach
        { l =>
        
            val els = l.split("\t").map( _.trim)
            val lat = els(1).toDouble
            val lon = els(2).toDouble
            val score = (els(3).toDouble - 1.0) / 9.0
            val picLink = els(6)
            val picIndex = picLink.split("/").last.toInt
            
            val c = new Coord( lat=lat, lon=lon )
            scenicMap.add( c, new ScenicPoint( c, score, picIndex ) )
        }
        
        val srtmFiles = new java.io.File( "./data" ).listFiles.filter( _.toString.endsWith(".asc") )
        log.info( "Found SRTM files: " + srtmFiles.toList )
        val heightMap = new SRTMInMemoryTiles( srtmFiles )
        
        log.info( "Building wikipedia cross-linked POIs" )
        val poiMap = new RTreeIndex[POI]()
        POIBuilder.build(map).foreach { poi => poiMap.add( poi.coord, poi ) }
        
        val rgFile = new java.io.File(mapFile + ".rg")
        log.info( "Building RoutableGraph" )
        val rgi = RoutableGraphBuilder( map, scenicMap, heightMap, poiMap )
        
        log.info( "Saving graph to: " + rgFile.toString )
        RoutableGraphBuilder.save( rgi, rgFile )
        
        
        log.info( "Loading graph to check serialization logic")
        val rg = RoutableGraphBuilder.load( rgFile )
        
        log.info( "Complete..." )
    }
}

