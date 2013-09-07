package org.seacourt.osm.route

import org.seacourt.osm._

import scala.collection.{mutable, immutable}


object RoutableGraphBuilder extends Logging
{
    import java.io._
    import java.util.zip._
    
    def save( rg : RoutableGraph, output : File )
    {
        val ops = new DataOutputStream( new GZIPOutputStream( new BufferedOutputStream( new FileOutputStream( output ) ) ) )
        try
        {
            ops.writeInt( rg.strings.size )
            for ( str <- rg.strings )
            {
                ops.writeUTF( str )
            }
        
            ops.writeInt( rg.nodes.size )
            for ( n <- rg.nodes )
            {
                ops.writeInt( n.nodeId )
                ops.writeDouble( n.coord.lon )
                ops.writeDouble( n.coord.lat )
                ops.writeFloat( n.height )
            }
            
            ops.writeInt( rg.scenicPoints.size )
            for ( sp <- rg.scenicPoints )
            {
                ops.writeDouble( sp.coord.lon )
                ops.writeDouble( sp.coord.lat )
                ops.writeDouble( sp.score )
                ops.writeInt( sp.picIndex )
            }
            
            for ( n <- rg.nodes )
            {
                ops.writeInt( n.nodeId )
                ops.writeInt( n.destinations.size )
                for ( (destNode, edge) <- n.destinations )
                {
                    ops.writeInt( destNode.nodeId )
                    ops.writeDouble( edge.dist )
                    ops.writeDouble( edge.cost )
                    ops.writeInt( edge.nameId )
                    ops.writeInt( edge.scenicPoints.size )
                    for ( sp <- edge.scenicPoints )
                    {
                        ops.writeInt( sp.picIndex )
                    }
                }
            }
        }
        finally
        {
            ops.close
        }
    }
    
    def load( input : File ) =
    {
        val ips = new DataInputStream( new GZIPInputStream( new BufferedInputStream( new FileInputStream( input ) ) ) )
        
        try
        {
            val numStrings = ips.readInt
            log.info( "Reading strings: " + numStrings )
            val strings = (0 until numStrings).map { _ => ips.readUTF() }
            
            
            val nodeMap = mutable.HashMap[Int, RouteNode]()
            val numNodes = ips.readInt
            log.info( "Reading node data for : " + numNodes )
            (0 until numNodes).foreach
            { _ =>
                val nodeId = ips.readInt
                val nodeLon = ips.readDouble
                val nodeLat = ips.readDouble
                val height = ips.readFloat
                
                val node = new RouteNode( nodeId, new Coord( nodeLon, nodeLat ), height )
                
                nodeMap += (nodeId -> node)
            }
            
            val numScenicPoints = ips.readInt
            val spMap = (0 until numScenicPoints).map
            { _ =>
                val spLon = ips.readDouble
                val spLat = ips.readDouble
                val score = ips.readDouble
                val picIndex = ips.readInt
                
                (picIndex -> new ScenicPoint( new Coord( spLon, spLat ), score, picIndex ))
            }.toMap
            
            log.info( "Reading edge data for : " + numNodes )
            (0 until numNodes).foreach
            { _ =>
                val sourceId = ips.readInt
                val numDests = ips.readInt
                
                (0 until numDests).foreach
                { _ =>
                    val destId = ips.readInt
                    val edgeDist = ips.readDouble
                    val edgeCost = ips.readDouble
                    val nameId = ips.readInt
                    
                    val numScenicPoints = ips.readInt
                    val sps = (0 until numScenicPoints).map
                    { _ =>
                    
                        val spIndex = ips.readInt
                        spMap(spIndex)
                    }
                    
                    val edge = new RouteEdge( edgeDist, edgeCost, nameId, sps.to[Array] )
                    
                    val sourceNode = nodeMap( sourceId )
                    val destNode = nodeMap( destId )                    
                    
                    sourceNode.addEdge( destNode, edge )
                }
            }
            
            log.info( "Read complete. Returning route graph" )
            new RoutableGraph( strings.toArray, nodeMap.map( _._2 ).toArray, spMap.map( _._2 ).toArray )
        }
        finally
        {
            ips.close
        }
    }
    
    def apply( osmMap : OSMMap, scenicMap : RTreeIndex[ScenicPoint], heightMap : SRTMInMemoryTiles ) =
    {
        // Find all non-synthetic nodes which belong to more than one way
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
                val realNodes = w.nodeIds.filter( nid => !osmMap.nodes(nid).synthetic ).toVector
                startEndNodes.append( realNodes.head )
                startEndNodes.append( realNodes.last )
                realNodes.foreach( nid => incCount(nid) )
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
        
        var nextStringId = 0
        val stringMap = mutable.HashMap[String, Int]()
        
        {
            val routeNodeMap = mutable.Map[Int, RouteNode]()
            
            var edgeCount = 0
            for ( w <- osmMap.ways )
            {
                val highwayAnnotation = w.tags
                    .map { t => (osmMap.tagRegistry.keyMap( t.keyId ), osmMap.tagRegistry.valMap( t.valueId )) }
                    .filter { _._1 == "highway" }
                    .map { _._2 }
                    .headOption
                    
                val junctionAnnotation = w.tags
                    .map { t => (osmMap.tagRegistry.keyMap( t.keyId ), osmMap.tagRegistry.valMap( t.valueId )) }
                    .filter { _._1 == "junction" }
                    .map { _._2 }
                    .headOption
                    
                val bridgeAnnotation = w.tags
                    .map { t => (osmMap.tagRegistry.keyMap( t.keyId ), osmMap.tagRegistry.valMap( t.valueId )) }
                    .filter { _._1 == "bridge" }
                    .map { _._2 }
                    .headOption
                    
                val nameAnnotation = w.tags
                    .map { t => (osmMap.tagRegistry.keyMap( t.keyId ), osmMap.tagRegistry.valMap( t.valueId )) }
                    .filter { _._1 == "name" }
                    .map { _._2 }
                    .headOption
                    
                val refAnnotation = w.tags
                    .map { t => (osmMap.tagRegistry.keyMap( t.keyId ), osmMap.tagRegistry.valMap( t.valueId )) }
                    .filter { _._1 == "ref" }
                    .map { _._2 }
                    .headOption
                    
                // Other important things:
                // ford: yes - In the case of Duxford Ford, this is not fordable.
                var costMultiplierOption = highwayAnnotation match
                {
                     case None => None
                     case Some( valueString ) =>
                     {
                        if ( valueString.startsWith( "motorway" ) ) Some( 10.0 )
                        else if ( valueString.startsWith( "trunk" ) ) Some( 1.8 )
                        else if ( valueString.startsWith( "primary" ) ) Some( 1.6 )
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
                    
                    val nameId = stringMap.getOrElseUpdate( name,
                    {
                        val nextId = nextStringId
                        nextStringId +=1
                        nextId
                    } )
                
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
                    var scenicPoints = mutable.ArrayBuffer[ScenicPoint]()
                    for ( nid <- w.nodeIds )
                    {
                        val isRouteNode = routeNodeIds contains nid
                        val node = osmMap.nodes(nid)
                        
                        val nearestScenicPoint = scenicMap.nearest(node.coord).get
                        
                        if ( nearestScenicPoint.coord.distFrom(node.coord) < 200.0 )
                        {
                            scenicPoints.append( nearestScenicPoint )
                            allSPs.add( nearestScenicPoint )
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
                            
                                val edge = new RouteEdge( dist, dist * costMultiplier * scenicScore * inclineScore, nameId, scenicPoints.distinct.toArray )
                                rn.addEdge( lrn, edge )
                                lrn.addEdge( rn, edge )
                                edgeCount += 1
                            }
                            
                            lastRouteNode = Some(rn)
                            scenicPoints.clear()
                            dist = 0.0
                            absHeightDelta = 0.0
                        }
                        
                        lastNode = Some(node)
                    }
                }
            }
            
            log.info( "Number of osm nodes: %d, number of route nodes: %d and edges: %d".format( osmMap.nodes.size, routeNodeMap.size, edgeCount ) )
            
            val stringArray = stringMap
                .map( _.swap )
                .toSeq
                .sortBy( _._1 )
                .map( _._2 )
                .toArray
            
            new RoutableGraph( stringArray, routeNodeMap.map { _._2 }.toArray, allSPs.toArray )
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
        println( "Found SRTM files: " + srtmFiles.toList )
        val heightMap = new SRTMInMemoryTiles( srtmFiles )
        
        val rgFile = new java.io.File(mapFile + ".rg")
        log.info( "Building RoutableGraph" )
        val rgi = RoutableGraphBuilder( map, scenicMap, heightMap )
        
        log.info( "Saving graph to: " + rgFile.toString )
        RoutableGraphBuilder.save( rgi, rgFile )
        
        
        log.info( "Loading graph to check serialization logic")
        val rg = RoutableGraphBuilder.load( rgFile )
        
        log.info( "Complete..." )
    }
}
