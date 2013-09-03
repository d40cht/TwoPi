package org.seacourt.osm.route

import org.seacourt.osm._

import scala.collection.{mutable, immutable}

case class ScenicPoint( coord : Coord, score : Double, picIndex : Long )
{
    assert( score >= 0.0 && score <= 1.0 )
}

case class RouteNode( val nodeId : Int, val coord : Coord )
{
    val destinations = mutable.ArrayBuffer[(RouteNode, RouteEdge)]()
    
    def addEdge( dest : RouteNode, edge : RouteEdge ) =
    {
        destinations.append( (dest, edge) )
    }
}

case class RouteEdge( val dist : Double, val cost : Double, val scenicPoints : Array[ScenicPoint] )


case class PathElement( ra : RouteAnnotation, re : Option[RouteEdge] )

case class RouteAnnotation( val node : RouteNode, var cost : Double, var dist : Double )
{
    var parent : Option[PathElement]    = None
}

class RTreeIndex[T]
{
    import com.infomatiq.jsi.{Rectangle, Point}
    import com.infomatiq.jsi.rtree.RTree
    
    val index = new RTree()
    index.init(null)
    
    val objMap = mutable.ArrayBuffer[T]()
    
    def add( c : Coord, value : T )
    {
        val thisId = objMap.size
        objMap.append( value )
        index.add( new Rectangle( c.lon.toFloat, c.lat.toFloat, c.lon.toFloat, c.lat.toFloat ), thisId )
    }
    
    def nearest( c : Coord, n : Int ) : Seq[T] =
    {
        val ids = mutable.ArrayBuffer[Int]()
        
        index.nearestN(
            new Point( c.lon.toFloat, c.lat.toFloat ),
            new gnu.trove.TIntProcedure
            {
                def execute( id : Int ) =
                {
                    ids.append(id)
                    true
                }
            },
            n,
            Float.MaxValue )
            
        ids.map( id => objMap(id) )
    }
    
    def nearest( c : Coord ) : Option[T] = nearest(c, 1).headOption
}

case class RouteResult( routeNodes : Seq[RouteNode], picList : Seq[ScenicPoint] )

class RoutableGraph( val nodes : Array[RouteNode] )
{
    def getClosest( coord : Coord ) : RouteNode =
    {
        // Horribly inefficient. Use an RTree shortly, or use IndexedMap as a starting point
        var minDist : Option[(Double, RouteNode)] = None
        nodes.foreach
        { rn => 
            val d = rn.coord.distFrom( coord )
            
            if ( minDist.isEmpty || d < minDist.get._1 ) minDist = Some( (d, rn) )
        }
        
        minDist.get._2
    }

    def runDijkstra( startNode : RouteNode, maxDist : Double, random : util.Random ) : mutable.HashMap[Int, RouteAnnotation] =
    {
        val sn = RouteAnnotation( startNode, 0.0, 0.0 )
        val visited = mutable.HashSet[Int]()
        val annotations = mutable.HashMap( startNode.nodeId -> sn )
        
        implicit val raOrdering = Ordering.fromLessThan( (ra1 : RouteAnnotation, ra2 : RouteAnnotation) =>
        {
            if ( ra1.cost != ra2.cost ) ra1.cost < ra2.cost
            else ra1.node.nodeId < ra2.node.nodeId
        } )
        
        val edgeCostMultipliers = mutable.HashMap[RouteEdge, Double]()
        
        var q = immutable.TreeSet[RouteAnnotation](sn)
     
        while ( !q.isEmpty )
        {
            val minEl = q.head
            q -= minEl
            
            visited.add(minEl.node.nodeId)
            
            //println( minEl.node.nodeId )
            
            minEl.node.destinations.foreach
            { case (node, edge) =>
                
                if ( !visited.contains(node.nodeId) )
                {
                    val ecm = 1.0//edgeCostMultipliers.getOrElseUpdate( edge, (1.0 + 0.1*random.nextGaussian) )
                    val nodeAnnot = annotations.getOrElseUpdate( node.nodeId, RouteAnnotation( node, Double.MaxValue, Double.MaxValue ) )
                    val thisCost = minEl.cost + (edge.cost * ecm)
                    val thisDist = minEl.dist + edge.dist
                    
                    //println( nodeAnnot.cost, thisCost )
                    
                    if ( nodeAnnot.cost > thisCost && thisDist < maxDist )
                    {
                        q -= nodeAnnot
                        
                        nodeAnnot.cost = thisCost
                        nodeAnnot.dist = minEl.dist + edge.dist
                        nodeAnnot.parent = Some( PathElement(minEl, Some(edge)) )
                        
                        q += nodeAnnot
                    }
                }
            }
        }
        
        annotations
    }
    
    def quantiseCoord( c : Coord ) =
    {
        def q( v : Double ) = ((v * 50.0).toInt).toDouble / 50.0
        
        new Coord( lon = q(c.lon), lat = q(c.lat) )
    }
    
    def buildRoute( startNode : RouteNode, targetDist : Double, seed : Int ) : RouteResult =
    {
        val random = new util.Random( seed )
        val startAnnotation = runDijkstra( startNode, targetDist, random )
        
        println( "Computing distances from start node" )

        val allDestinationsRaw = startAnnotation
            .filter { case (nid, annot) => annot.dist > targetDist * 0.2 && annot.dist < targetDist * 0.6 }
            .toSeq
            .sortBy { case (nid, annot) => annot.cost }
            
        val allDestinations = allDestinationsRaw
            .groupBy { case (n, annot) => quantiseCoord( annot.node.coord ) }
            .map { case (c, allPoints) => allPoints.sortBy( _._2.cost ).head }
            
            
        // Choose randomly from the top 50% by cost
        val candidateDestinations = allDestinations
            .take( allDestinations.size / 2 )
            .toVector
            
        val elementIndex = random.nextInt( candidateDestinations.size )
        
        val annot1 = candidateDestinations(elementIndex)._2
        
        println( "Computing distances from second node" )
        val node2Annotation = runDijkstra( annot1.node, targetDist, random )
        
        
        println( "Computing possible midpoints" )
        val possibleMidPoints = node2Annotation
            // Add distance annotation from the start node
            .filter { case (nid, annot) => startAnnotation contains nid }
            .map { case (nid, annot) => (nid, startAnnotation(nid), annot ) }
            .filter { case (nid, annot1, annot2) => (annot1.dist + annot2.dist) < 0.8 * targetDist }
            .toSeq
            .sortBy { case (nid, annot1, annot2) => (annot1.cost + annot2.cost) }
            .toSeq
            
        
        val trimmedMidPoints = possibleMidPoints
            .take( possibleMidPoints.size )
            
        val trimmedSample1 = random.shuffle(trimmedMidPoints).take( 200 )
        val trimmedSample2 = random.shuffle(trimmedMidPoints).take( 200 )
            
        
            
        def routeNodes( id1 : Int, id2 : Int ) : Seq[PathElement] =
        {
            def traceBack( endNode : RouteAnnotation ) : Seq[PathElement] =
            {
                val routeAnnotations = mutable.ArrayBuffer[PathElement]()
                
                var iterNode : Option[PathElement] = Some( PathElement(endNode, None) )
                
                do
                {
                    val PathElement(ra, re) = iterNode.get
                    routeAnnotations.append( PathElement(ra, re) )
                    iterNode = ra.parent
                }
                while ( iterNode != None )
                
                routeAnnotations
            }
            
            traceBack( startAnnotation(id1) ).reverse ++
            traceBack( node2Annotation(id1) ) ++
            traceBack( node2Annotation(id2) ).reverse ++
            traceBack( startAnnotation(id2) )
        }
        
        // Enumerate all pairs of midpoints that sum to roughly the target distance
        println( "Enumerating mid points (%d*%d)".format( trimmedSample1.size, trimmedSample2.size ) )
        val possibleMidPointPairs = trimmedSample1.flatMap
        { case (nid1, annot11, annot12) =>
        
            trimmedSample2
                .filter
                {
                    case (nid2, annot21, annot22) =>
                    
                    val routeDist = annot11.dist + annot12.dist + annot21.dist + annot22.dist
                    
                    ( (nid1 != nid2) && (routeDist > (targetDist * 0.8)) && (routeDist < (targetDist * 1.2)) )
                }
                //.map { case (nid2, annot21, annot22) => (nid1, nid2, annot11.cost + annot12.cost + annot21.cost + annot22.cost) }
                .map
                { case (nid2, annot21, annot22) =>
                
                    val routeNodeIds = routeNodes( nid1, nid2 ).map( _.ra.node.nodeId )
                    val circularityRatio = routeNodeIds.toSet.size.toDouble / routeNodeIds.size.toDouble
                    
                    val cost = annot11.cost + annot12.cost + annot21.cost + annot22.cost
                    val routeDist = annot11.dist + annot12.dist + annot21.dist + annot22.dist;
                    //val relativeDist = annot21.coord.distFrom( annot11.coord ) / targetDist
                    
                    // Upweight routes where nid1 and nid2 are farther apart
                    (nid1, nid2, cost, circularityRatio, routeDist, annot11, annot12, annot21, annot22)
                }
        }
        .filter { _._4 > 0.8 }
        //.sortBy( x => x._3 / x._4 )
        .sortBy( x => x._3 )
        .toVector
        
        println( "Possible mid-point pairs: " + possibleMidPointPairs.size )
        
        val chosenPairIndex = random.nextInt( possibleMidPointPairs.size / 2 )

        // Find the best pair by cumulative cost
        val (bestId1, bestId2, cost, circularityRatio, routeDist, annot11, annot12, annot21, annot22) = possibleMidPointPairs(chosenPairIndex)
        
        
        println( "Route has distance: %.2fkm, cost: %.2f, circularity ratio: %f".format( routeDist / 1000.0, cost / 1000.0, circularityRatio ) )
        println( annot11.dist, annot12.dist, annot21.dist, annot22.dist )
        
        
        
        // Now the route is:
        // * startNode -> best1 -> annot1 -> best2 -> startNode. Enumerate
        // the coordinates on the way.
        
        val best1 = startAnnotation(bestId1)
        val best2 = startAnnotation(bestId2)
        println( startNode.coord.lat + ", " + startNode.coord.lon )
        println( annot1.node.coord.lat + ", " + annot1.node.coord.lon )
        println( best1.node.coord.lat + ", " + best1.node.coord.lon )
        println( best2.node.coord.lat + ", " + best2.node.coord.lon )
        
        val fullRoute = routeNodes( bestId1, bestId2 )

        val nodeList = fullRoute.map( _.ra.node )
        val edgeList = fullRoute.flatMap( _.re )
        
        val pics = edgeList
            .flatMap( _.scenicPoints )
            .distinct
            .filter( _.score > 0.3 )
            .sortBy( -_.score )
            .take(10)
        
        new RouteResult( nodeList, pics )
    }
}


object RoutableGraph
{
    import java.io._
    
    def save( rg : RoutableGraph, output : File )
    {
        val ops = new DataOutputStream( new BufferedOutputStream( new FileOutputStream( output ) ) )
        try
        {
            ops.writeInt( rg.nodes.size )
            
            for ( n <- rg.nodes )
            {
                ops.writeInt( n.nodeId )
                ops.writeDouble( n.coord.lon )
                ops.writeDouble( n.coord.lat )
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
                    ops.writeInt( edge.scenicPoints.size )
                    for ( sp <- edge.scenicPoints )
                    {
                        ops.writeDouble( sp.coord.lon )
                        ops.writeDouble( sp.coord.lat )
                        ops.writeDouble( sp.score )
                        ops.writeLong( sp.picIndex )
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
        val ips = new DataInputStream( new BufferedInputStream( new FileInputStream( input ) ) )
        
        try
        {
            val nodeMap = mutable.HashMap[Int, RouteNode]()
            val numNodes = ips.readInt
            println( "Reading node data for : " + numNodes )
            (0 until numNodes).foreach
            { _ =>
                val nodeId = ips.readInt
                val nodeLon = ips.readDouble
                val nodeLat = ips.readDouble
                
                val node = new RouteNode( nodeId, new Coord( nodeLon, nodeLat ) )
                
                nodeMap += (nodeId -> node)
            }
            
            println( "Reading edge data for : " + numNodes )
            (0 until numNodes).foreach
            { _ =>
                val sourceId = ips.readInt
                val numDests = ips.readInt
                
                (0 until numDests).foreach
                { _ =>
                    val destId = ips.readInt
                    val edgeDist = ips.readDouble
                    val edgeCost = ips.readDouble
                    
                    val numScenicPoints = ips.readInt
                    val sps = (0 until numScenicPoints).map
                    { _ =>
                    
                        val spLon = ips.readDouble
                        val spLat = ips.readDouble
                        val score = ips.readDouble
                        val picIndex = ips.readLong
                        
                        new ScenicPoint( new Coord( spLon, spLat ), score, picIndex )
                    }
                    
                    val edge = new RouteEdge( edgeDist, edgeCost, sps.to[Array] )
                    
                    val sourceNode = nodeMap( sourceId )
                    val destNode = nodeMap( destId )                    
                    
                    sourceNode.addEdge( destNode, edge )
                }
            }
            
            new RoutableGraph( nodeMap.map( _._2 ).toArray )
        }
        finally
        {
            ips.close
        }
    }
    
    def apply( osmMap : OSMMap, scenicMap : RTreeIndex[ScenicPoint] ) =
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
            
            println( "Num nodes: %d".format( nodeWayMembershipCount.size ) )
            
            val junctionNodes = nodeWayMembershipCount
                .filter { case (nid, count) => count > 1 }
                .map { case (nid, count) => nid }
                .toSet
                
            println( "Num junction nodes: %d and start/end nodes: %d".format( junctionNodes.size, startEndNodes.size ) )
                
            (junctionNodes ++ startEndNodes).toSet
        }
        
        // Build the route graph
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
                    
                val nameAnnotation = w.tags
                    .map { t => (osmMap.tagRegistry.keyMap( t.keyId ), osmMap.tagRegistry.valMap( t.valueId )) }
                    .filter { _._1 == "ref" }
                    .map { _._2 }
                    .headOption
                
                var costMultiplier = highwayAnnotation match
                {
                     case None => 1.0
                     case Some( valueString ) =>
                     {
                        if ( valueString.startsWith( "motorway" ) ) 10.0
                        else if ( valueString.startsWith( "bridleway" ) ) 1.0
                        else if ( valueString.startsWith( "track" ) ) 1.0
                        else if ( valueString.startsWith( "residential" ) ) 1.5
                        else if ( valueString.startsWith( "trunk" ) ) 1.8
                        // Not yet classified, so be conservative
                        else if ( valueString.startsWith( "primary" ) ) 1.2
                        //else if ( valueString.startsWith( "service" ) ) 1.1
                        else if ( valueString.startsWith( "road" ) ) 1.0
                        else if ( valueString.startsWith( "secondary" ) ) 1.0
                        else if ( valueString.startsWith( "tertiary" ) ) 1.0
                        else if ( valueString.startsWith( "unclassified" ) ) 1.0
                        else if ( valueString.startsWith( "cycleway" ) ) 1.2
                        else if ( valueString.startsWith( "bridleway" ) ) 0.9
                        else if ( valueString.startsWith( "footway" ) ) 0.9
                        else if ( valueString.startsWith( "footpath" ) ) 0.9
                        else 100.0
                     }
                }
                
                nameAnnotation.foreach
                { n =>
                    if (n.matches("A[0-9]+"))
                    {
                        costMultiplier = 1.5
                    }
                }
                
                
                var dist = 0.0
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
                    }
                    
                    // Update cumulative way distance
                    lastNode.foreach
                    { ln =>
                    
                        dist += ln.coord.distFrom( node.coord )
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
                        
                        val rn = routeNodeMap.getOrElseUpdate( nid, new RouteNode(nid, node.coord) )
                        
                        lastRouteNode.foreach
                        { lrn =>
                        
                            val edge = new RouteEdge( dist, dist * costMultiplier * scenicScore, scenicPoints.distinct.toArray )
                            rn.addEdge( lrn, edge )
                            lrn.addEdge( rn, edge )
                            edgeCount += 1
                        }
                        
                        lastRouteNode = Some(rn)
                        scenicPoints.clear()
                        dist = 0.0
                    }
                    
                    lastNode = Some(node)
                }
            }
            
            println( "Number of osm nodes: %d, number of route nodes: %d and edges: %d".format( osmMap.nodes.size, routeNodeMap.size, edgeCount ) )
            new RoutableGraph( routeNodeMap.map { _._2 }.toArray )
        }
    }
}

object GenerateRoute extends App
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
            val picIndex = picLink.split("/").last.toLong
            
            val c = new Coord( lat=lat, lon=lon )
            scenicMap.add( c, new ScenicPoint( c, score, picIndex ) )
        }
        
        val rgFile = new java.io.File(mapFile + ".rg")
        val rgi = RoutableGraph( map, scenicMap )
        RoutableGraph.save( rgi, rgFile )
        
        val rg = RoutableGraph.load( rgFile )
        
        for ( ln <- io.Source.stdin.getLines )
        {
            try
            {
                val els = ln.split(" ")
                
                val startCoords = Coord( els(0).toDouble, els(1).toDouble )
                val distInkm = els(2).toDouble
                val seed = els(3).toInt
                
                println( "Finding closest node..." )
                val closestNode = rg.getClosest( startCoords )
                
                println( "Closest: " + closestNode.coord )
                
                val routeNodes = rg.buildRoute( closestNode, distInkm * 1000.0, seed ).routeNodes
                
                for ( rn <- routeNodes )
                {
                    println( "%f, %f".format( rn.coord.lat, rn.coord.lon ) )
                }
            }
            catch
            {
                case e : java.lang.Throwable => println( "Unhandled exception: " + e )
            }
        }
    }
}
