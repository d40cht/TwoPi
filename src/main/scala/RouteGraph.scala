package org.seacourt.osm

import scala.collection.{mutable, immutable}

case class RouteNode( val id : Int )
{
    def destinations = Seq[(RouteEdge, RouteNode)]()
}

case class RouteEdge( val dist : Double, val cost : Double )

case class RouteAnnotation( val node : RouteNode, var cost : Double )
{
    var dist                        = 0.0
    var parent : Option[RouteNode]  = None
}


class RoutableGraph( val nodes : Array[RouteNode] )
{
    def runDijkstra( startNode : RouteNode ) =
    {
        val sn = RouteAnnotation( startNode, 0.0 )
        val annotations = mutable.HashMap( startNode -> sn )
        
        implicit val raOrdering = Ordering.fromLessThan( (ra1 : RouteAnnotation, ra2 : RouteAnnotation) =>
        {
            if ( ra1.cost != ra2.cost ) ra1.cost < ra2.cost
            else ra1.node.id < ra2.node.id
        } )
        
        var q = immutable.TreeSet[RouteAnnotation](sn)
     
        while ( !q.isEmpty )
        {
            val minEl = q.head
            q -= minEl
            
            minEl.node.destinations.foreach
            { case (edge, node) =>
                
                val nodeAnnot = annotations.getOrElseUpdate( node, RouteAnnotation( node, Double.MaxValue ) )
                val thisCost = minEl.cost + edge.cost
                
                if ( nodeAnnot.cost > thisCost )
                {
                    q -= nodeAnnot
                    
                    nodeAnnot.cost = thisCost
                    nodeAnnot.dist = minEl.dist + edge.dist
                    nodeAnnot.parent = Some( minEl.node )
                    
                    q += nodeAnnot
                }
            }
        }
        
        annotations
    }
    
    
    def buildRoute( startNode : RouteNode, targetDist : Double ) =
    {
        val startAnnotation = runDijkstra( startNode )
        
        // First remote node is between 1/4 and 3/8 of total dist and of min cost
        val annot1 = startAnnotation
            .filter { case (n, annot) => annot.dist > targetDist * 0.25 && annot.dist < targetDist * 0.375 }
            .toSeq
            .sortBy { case (n, annot) => annot.cost }
            .head
            ._2
            
        
        val node2Allocation = runDijkstra( annot1.node )
        
        val possibleMidPoints = node2Allocation
            // Add distance annotation from the start node
            .map { case (n, annot) => (n, annot1, startAnnotation(n) ) }
            // Total distance must be less than 3/4 of the full distance
            .filter { case (n, annot1, annot2) => (annot1.dist + annot2.dist) < 0.75 * targetDist }
            .toSeq
        
        // Enumerate all pairs of midpoints that sum to roughly the target distance
        val possibleMidPointPairs = possibleMidPoints.flatMap
        { case (n1, annot11, annot12) =>
        
            possibleMidPoints
                .filter
                {
                    case (n2, annot21, annot22) =>
                    
                    val routeDist = annot11.dist + annot12.dist + annot21.dist + annot22.dist;
                    
                    ( (n1 != n2) && (routeDist > (targetDist * 0.8)) && (routeDist < (targetDist * 1.2)) )
                }
                .map { case (n2, annot21, annot22) => (n1, n2, annot11.cost + annot12.cost + annot21.cost + annot22.cost) }
        }
        .sortBy( _._3 )

        // Find the best pair by cumulative cost
        val (best1, best2, cost) = possibleMidPoints.head
        
        // Now the route is startNode -> best1 -> annot1 -> best2 -> startNode
            
    }
}

