package org.seacourt.osm.route

import org.seacourt.osm._

import scala.collection.{mutable, immutable}

case class RouteNode( val nodeId : Int, val node : Node )
{
    val destinations = mutable.ArrayBuffer[(RouteNode, RouteEdge)]()
    
    def addEdge( dest : RouteNode, edge : RouteEdge ) =
    {
        destinations.append( (dest, edge) )
    }
}

case class RouteEdge( val dist : Double, val cost : Double )

case class RouteAnnotation( val node : RouteNode, var cost : Double )
{
    var dist                                = 0.0
    var parent : Option[RouteAnnotation]    = None
}


class RoutableGraph( val osmMap : OSMMap, val nodes : Array[RouteNode] )
{
    def getClosest( coord : Coord ) : RouteNode =
    {
        // Horribly inefficient. Use an RTree shortly, or use IndexedMap as a starting point
        nodes
            .sortBy( rn => rn.node.coord.distFrom( coord ) )
            .head
    }

    def runDijkstra( startNode : RouteNode ) : mutable.HashMap[Int, RouteAnnotation] =
    {
        val sn = RouteAnnotation( startNode, 0.0 )
        val visited = mutable.HashSet[Int]()
        val annotations = mutable.HashMap( startNode.nodeId -> sn )
        
        implicit val raOrdering = Ordering.fromLessThan( (ra1 : RouteAnnotation, ra2 : RouteAnnotation) =>
        {
            if ( ra1.cost != ra2.cost ) ra1.cost < ra2.cost
            else ra1.node.nodeId < ra2.node.nodeId
        } )
        
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
                    val nodeAnnot = annotations.getOrElseUpdate( node.nodeId, RouteAnnotation( node, Double.MaxValue ) )
                    val thisCost = minEl.cost + edge.cost
                    
                    //println( nodeAnnot.cost, thisCost )
                    
                    if ( nodeAnnot.cost > thisCost )
                    {
                        q -= nodeAnnot
                        
                        nodeAnnot.cost = thisCost
                        nodeAnnot.dist = minEl.dist + edge.dist
                        nodeAnnot.parent = Some( minEl )
                        
                        q += nodeAnnot
                    }
                }
            }
        }
        
        annotations
    }
    
    
    def buildRoute( startNode : RouteNode, targetDist : Double ) : Seq[RouteNode] =
    {
        val startAnnotation = runDijkstra( startNode )
        
        println( "Computing distances from start node" )
        
        // First remote node is between 1/4 and 3/8 of total dist and of min cost
        val annot1 = startAnnotation
            .filter { case (n, annot) => annot.dist > targetDist * 0.25 && annot.dist < targetDist * 0.375 }
            .toSeq
            .sortBy { case (n, annot) => annot.cost }
            .head
            ._2
            
        
        println( "Computing distances from second node" )
        val node2Annotation = runDijkstra( annot1.node )
        
        
        println( "Computing possible midpoints" )
        val possibleMidPoints = node2Annotation
            // Add distance annotation from the start node
            .map { case (nid, annot) => (nid, annot1, startAnnotation(nid) ) }
            // Total distance must be less than 3/4 of the full distance
            .filter { case (nid, annot1, annot2) => (annot1.dist + annot2.dist) < 0.75 * targetDist }
            .toSeq
        
        // Enumerate all pairs of midpoints that sum to roughly the target distance
        println( "Enumerating mid points (%d*%d)".format( possibleMidPoints.size, possibleMidPoints.size ) )
        val possibleMidPointPairs = possibleMidPoints.flatMap
        { case (nid1, annot11, annot12) =>
        
            possibleMidPoints
                .filter
                {
                    case (nid2, annot21, annot22) =>
                    
                    val routeDist = annot11.dist + annot12.dist + annot21.dist + annot22.dist;
                    
                    ( (nid1 != nid2) && (routeDist > (targetDist * 0.8)) && (routeDist < (targetDist * 1.2)) )
                }
                .map { case (nid2, annot21, annot22) => (nid1, nid2, annot11.cost + annot12.cost + annot21.cost + annot22.cost) }
        }
        .sortBy( _._3 )

        // Find the best pair by cumulative cost
        val (bestId1, bestId2, cost) = possibleMidPointPairs.head
        
        
        def traceBack( endNode : RouteAnnotation ) : Seq[RouteAnnotation] =
        {
            val routeAnnotations = mutable.ArrayBuffer[RouteAnnotation]()
            
            var iterNode : Option[RouteAnnotation] = Some(endNode)
            do
            {
                routeAnnotations.append( iterNode.get )
                iterNode = iterNode.get.parent
            }
            while ( iterNode != None )
            
            routeAnnotations
        }
        
        // Now the route is:
        // * startNode -> best1 -> annot1 -> best2 -> startNode. Enumerate
        // the coordinates on the way.
        
        val fullRoute =
            traceBack( startAnnotation(bestId1) ).reverse ++
            traceBack( node2Annotation(bestId1) ) ++
            traceBack( node2Annotation(bestId2) ).reverse ++
            traceBack( startAnnotation(bestId2) )

        fullRoute.map( _.node )
    }
}

object RoutableGraph
{
    def apply( osmMap : OSMMap ) =
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
                var dist = 0.0
                var lastNode : Option[Node] = None
                var lastRouteNode : Option[RouteNode] = None
                for ( nid <- w.nodeIds )
                {
                    val isRouteNode = routeNodeIds contains nid
                    val node = osmMap.nodes(nid)
                    
                    // Update cumulative way distance
                    lastNode.foreach
                    { ln =>
                    
                        dist += ln.coord.distFrom( node.coord )
                    }
                    
                    if ( isRouteNode )
                    {
                        val rn = routeNodeMap.getOrElseUpdate( nid, new RouteNode(nid, node) )
                        
                        lastRouteNode.foreach
                        { lrn =>
                        
                            val edge = new RouteEdge( dist, dist )
                            rn.addEdge( lrn, edge )
                            lrn.addEdge( rn, edge )
                            edgeCount += 1
                        }
                        
                        lastRouteNode = Some(rn)
                        dist = 0.0
                    }
                    
                    lastNode = Some(node)
                }
            }
            
            println( "Number of osm nodes: %d, number of route nodes: %d and edges: %d".format( osmMap.nodes.size, routeNodeMap.size, edgeCount ) )
            new RoutableGraph( osmMap, routeNodeMap.map { _._2 }.toArray )
        }
    }
}

object GenerateRoute extends App
{    
    override def main( args : Array[String] )
    {
        val mapFile = new java.io.File( args(0) )
        val startCoords = Coord( args(1).toDouble, args(2).toDouble )
        val distInkm = args(3).toDouble
        
        val map = OSMMap.load( mapFile )
        val rg = RoutableGraph( map )
        
        val closestNode = rg.getClosest( startCoords )
        
        println( "Closest: " + closestNode.node.coord )
        
        val routeNodes = rg.buildRoute( closestNode, distInkm * 1000.0 )
        
        for ( rn <- routeNodes )
        {
            println( "%f, %f".format( rn.node.coord.lat, rn.node.coord.lon ) )
        }
    }
}

