package org.seacourt.osm.route

import org.seacourt.osm._

import scala.collection.{mutable, immutable}


import scala.util.control.Breaks._


// Changes to algo:

// Stay at height when going between peaks?  I like this one, though it would make more sense to go along the ridge over Haystacks rather than drop down to the path in Ennerdale: http://two-pi.co.uk/displayroute?routeId=748652F12DD88699517C2D7390A18CDC

// Icons for POIs: http://www.sjjb.co.uk/mapicons/contactsheet#tourist


case class WikiLocated( name : String, coord : Coord, imageUrl : Option[String], rdfTypes : Set[String] )//, description : Option[String] )

// Load these from disk, or use the type system as below?
trait POIType
{
    def name : String
    def icon : java.io.File
}


case class POI(
    // From OSM
    val coord : Coord,
    // From OSM
    val name : String,
    // Change to some kind of enum. From OSM largely?
    val poiType : POIType,
    val wikiData : Option[WikiLocated] )
{
}

case class ScenicPoint( coord : Coord, score : Float, photographer : String, title : String, picIndex : Int, imgHash : String )
{
    assert( score >= 0.0 && score <= 1.0 )
}

case class EdgeDest( val node : RouteNode, val edge : RouteEdge )
{
}

case class RouteNode( val nodeId : Int, val node : Node )
{
    def coord = node.coord
    def height = node.height
    
    val destinations = mutable.ArrayBuffer[EdgeDest]()
    
    def addEdge( dest : RouteNode, edge : RouteEdge ) =
    {
        destinations.append( EdgeDest(dest, edge) )
    }
}



// TODO: There should really be a height delta on RouteEdge to get the costs right for long routes.
// but then we'd need to know which way we were going - so instate when doing one-way logic.

case class NearbyPOI( val dist : Float, val poi : POI )
{
}

case class RouteEdge(
    val edgeId : Int,
    val dist : Double,
    val cost : Double,
    val name : String,
    val scenicPoints : Array[ScenicPoint],
    val pois : Array[NearbyPOI],
    val nodes : Array[Node] )
{
}

case class EdgeAndBearing( val edge : RouteEdge, val bearing : Float )

case class PathElement( ra : RouteAnnotation, re : Option[EdgeAndBearing] )
{
    def edgeNodes : Seq[Node] =
    {
        re match
        {
            case None => Seq()
            case Some( eab ) =>
            {
                val rawNodes = eab.edge.nodes
                if ( rawNodes.last == ra.node.node ) rawNodes
                else rawNodes.reverse
            }
        }
    }
}

case class RouteAnnotation( val node : RouteNode, var cost : Double, var dist : Double )
{
    var parent : Option[PathElement]    = None
}

case class NodeAndDistance( val node : Node, val distance : Double )

case class RouteDirections( val inboundNodes : Array[NodeAndDistance], val inboundPics : Array[ScenicPoint], val inboundPOIs : Array[POI], val edgeName : String, val dist : Double, val cumulativeDistance : Double, val elevation : Double, bearing : Float, coord : Coord )

case class DebugPoint( coord : Coord, name : String )

case class RouteResult(
    directions : Array[RouteDirections],
    distance : Double,
    ascent : Double,
    debugPoints : Array[DebugPoint] )



case class RouteSegment( val edgeName : String, val pathElements : Seq[PathElement], val bearing : Float )


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

class RoutableGraph( val nodes : Array[RouteNode], val scenicPoints : Array[ScenicPoint] ) extends Logging
{
    val treeMap = new RTreeIndex[RouteNode]()
    
    log.info( "Populating route node tree map for quick indexing.")
    nodes.foreach( n => treeMap.add( n.coord, n ) )
    log.info( "... complete." )
    
    def getClosest( coord : Coord ) : RouteNode = treeMap.nearest( coord ).get

    type AnnotationMap = mutable.HashMap[Int, RouteAnnotation]
    
    def traceBack( endNode : RouteAnnotation, reverse : Boolean ) : Seq[PathElement] =
    {
        //val routeAnnotations = mutable.ArrayBuffer[PathElement]()
        
        val nodesRaw = mutable.ArrayBuffer[RouteAnnotation]()
        val edgesBearingsRaw = mutable.ArrayBuffer[EdgeAndBearing]()
        
        var iterNode : Option[PathElement] = Some( PathElement(endNode, None) )
        do
        {
            val PathElement(ra, edgeAndBearing) = iterNode.get
            edgeAndBearing.foreach( eb => edgesBearingsRaw.append( eb ) )
            nodesRaw.append( ra )
            iterNode = ra.parent
        }
        while ( iterNode != None )
        
        val nodes = if (reverse) nodesRaw.reverse else nodesRaw
        val edgesBearings = if (reverse)
        {
            edgesBearingsRaw.reverse.map( eb => EdgeAndBearing( eb.edge, normaliseDegrees( eb.bearing - 180.0 ).toFloat ) )
        }
        else edgesBearingsRaw
        
        
        assert( nodes.size == edgesBearings.size+1 )
        nodes.zip( None +: edgesBearings.map( e => Some(e) ) ).map { case (n, e) => PathElement( n, e ) }
    }
    
    private def runDFS( startNode : RouteNode, queueOrdering : Ordering[RouteAnnotation], maxDist : Double, endNode : Option[RouteNode] ) : AnnotationMap =
    {
        val sn = RouteAnnotation( startNode, 0.0, 0.0 )
        val visited = mutable.HashSet[Int]()
        val annotations = mutable.HashMap( startNode.nodeId -> sn )
        
        implicit val raOrdering = queueOrdering
        
        var q = immutable.TreeSet[RouteAnnotation](sn)
        breakable
        {
            while ( !q.isEmpty )
            {
                val minEl = q.head
                q -= minEl
                
                if ( Some(minEl.node) == endNode ) break
                
                visited.add(minEl.node.nodeId)
                
                minEl.node.destinations.foreach
                { case EdgeDest(node, edge) =>
                    
                    if ( !visited.contains(node.nodeId) )
                    {
                        val nodeAnnot = annotations.getOrElseUpdate( node.nodeId, RouteAnnotation( node, Double.MaxValue, Double.MaxValue ) )
                        val thisCost = minEl.cost + edge.cost
                        val thisDist = minEl.dist + edge.dist
                        
                        if ( nodeAnnot.cost > thisCost && thisDist < maxDist )
                        {
                            q -= nodeAnnot
                            
                            nodeAnnot.cost = thisCost
                            nodeAnnot.dist = minEl.dist + edge.dist
                            
                            val bearing = minEl.node.coord.bearing( node.coord ).toFloat
                            nodeAnnot.parent = Some( PathElement(minEl, Some(EdgeAndBearing(edge, bearing))) )
                            
                            q += nodeAnnot
                        }
                    }
                }
            }
        }
        
        annotations
    }
    
    private def runDijkstra( startNode : RouteNode, maxDist : Double ) : AnnotationMap = runDFS(
        startNode,
        Ordering.fromLessThan( (ra1 : RouteAnnotation, ra2 : RouteAnnotation) =>
        {
            if ( ra1.cost != ra2.cost ) ra1.cost < ra2.cost
            else ra1.node.nodeId < ra2.node.nodeId
        } ),
        maxDist,
        None )
        
    private def runAStar( startNode : RouteNode, endNode : RouteNode ) : AnnotationMap =
    {
        runDFS(
            startNode,
            Ordering.fromLessThan( (ra1 : RouteAnnotation, ra2 : RouteAnnotation) =>
            {
                val r1dist = ra1.cost + ra1.node.coord.distFrom( endNode.coord )/2.0
                val r2dist = ra2.cost + ra2.node.coord.distFrom( endNode.coord )/2.0
                if ( r1dist != r2dist ) r1dist < r2dist
                else ra1.node.nodeId < ra2.node.nodeId
            } ),
            Double.MaxValue,
            Some(endNode) )
    }
        
    private def aStarShortestPath( startNode : RouteNode, endNode : RouteNode ) : Seq[PathElement] =
    {
        val annot = runAStar( startNode, endNode )
            
        val endNodeRA = annot(endNode.nodeId)
        traceBack( endNodeRA, true )
    }

    /*private def runDijkstraOld( startNode : RouteNode, maxDist : Double ) : AnnotationMap =
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
            
            minEl.node.destinations.foreach
            { case EdgeDest(node, edge) =>
                
                if ( !visited.contains(node.nodeId) )
                {
                    val nodeAnnot = annotations.getOrElseUpdate( node.nodeId, RouteAnnotation( node, Double.MaxValue, Double.MaxValue ) )
                    val thisCost = minEl.cost + edge.cost
                    val thisDist = minEl.dist + edge.dist
                    
                    if ( nodeAnnot.cost > thisCost && thisDist < maxDist )
                    {
                        q -= nodeAnnot
                        
                        nodeAnnot.cost = thisCost
                        nodeAnnot.dist = minEl.dist + edge.dist
                        
                        val bearing = minEl.node.coord.bearing( node.coord ).toFloat
                        nodeAnnot.parent = Some( PathElement(minEl, Some(EdgeAndBearing(edge, bearing))) )
                        
                        q += nodeAnnot
                    }
                }
            }
        }
        
        annotations
    }*/
    
    // Normalise to -180, +180
    private def normaliseDegrees( angle : Double ) : Double =
    {
        var angleIt = angle
        while ( angleIt < -180 ) angleIt = angleIt + 360.0
        while ( angleIt > 180 ) angleIt = angleIt - 360.0
        
        angleIt
    }

    def quantiseCoord( c : Coord ) =
    {
        val qq = 100.0
        def q( v : Double ) = ((v * qq).toInt).toDouble / qq
        
        new Coord( lon = q(c.lon), lat = q(c.lat) )
    }
    

    private def routePath( startPointAnnotationMap : AnnotationMap, midPointAnnotationMap : AnnotationMap, id1 : Int, id2 : Int, pruneDistalOverlap : Boolean ) : Seq[PathElement] =
    {
        val seg1 = traceBack( startPointAnnotationMap(id1), reverse=true )
        val seg2 = traceBack( midPointAnnotationMap(id1), reverse=false )
        val seg3 = traceBack( midPointAnnotationMap(id2), reverse=true )
        val seg4 = traceBack( startPointAnnotationMap(id2), reverse=false )
        
        val distalOverlap = if ( pruneDistalOverlap )
        {
            seg3.zip(seg2.reverse)
                .takeWhile { case ( pe1, pe2 ) => pe1.ra.node.nodeId == pe2.ra.node.nodeId }
                .size - 1
        }
        else 0
        
        seg1 ++ seg2.dropRight(distalOverlap) ++ seg3.drop(distalOverlap) ++ seg4
        
    }
    
    private def aStarRoutePath( nodes : Seq[RouteNode] ) : Seq[PathElement] =
    {
        nodes.sliding(2).flatMap { case Seq( from, to ) => aStarShortestPath( from, to ) }.toSeq
    }
    
    type QuarterPoint = (Int, RouteAnnotation, RouteAnnotation)
    type QuarterPointPair = (QuarterPoint, QuarterPoint)
    
    
    // *************** The main mechanics of route finding happens here ***************
    
    case class FoundRoute( val path : Seq[PathElement], val debugPoints : Seq[DebugPoint] )
    
    private def findRoute( startNode : RouteNode, midNodeOption : Option[RouteNode], targetDistHint : Double ) : Option[FoundRoute] =
    {
        val trimEnd = midNodeOption.isEmpty
        val targetDist = midNodeOption match
        {
            case Some(mn)   =>
            {
                4.0 * startNode.coord.distFrom( mn.coord )
            }
            case None       => targetDistHint
        }
        val random = util.Random
        val startPointAnnotationMap = runDijkstra( startNode, targetDist )
        
        log.info( "Computing distances from start node: " + startNode.coord + ", " + targetDist )

        val allDestinationsRaw = startPointAnnotationMap
            .filter { case (nid, annot) => annot.dist > targetDist * 0.05 && annot.dist < targetDist * 0.25 }
            .toSeq
            .sortBy { case (nid, annot) => annot.cost/annot.dist }
            
        // Group destinations by quantised coordinates in order to ameliorate the bias towards
        // locations that have more ways per unit area.
        /*val allDestinations = allDestinationsRaw
            .groupBy { case (n, annot) => quantiseCoord( annot.node.coord ) }
            .map { case (c, allPoints) => allPoints.sortBy( p => p._2.cost / p._2.dist ).head }
            .toSeq
            .sortBy( p => p._2.cost / p._2.dist )
        
            
        log.info( "Number of quantised destination points in search radius: " + allDestinations.size )
            
        val candidateDestinations = allDestinations
                .take( allDestinations.size / 4 )
                .toVector*/
                
        val candidateDestinationsSeen = mutable.ArrayBuffer[RouteAnnotation]()
        val candidateDestinations = allDestinationsRaw.iterator.flatMap
        { ra =>
            val theRA = ra._2
            val dists = candidateDestinationsSeen.map( _.node.coord.distFrom( theRA.node.coord ) )
            if ( dists.isEmpty || dists.min > targetDist / 50.0 )
            {
                candidateDestinationsSeen.append( theRA )
                
                Some( ra )
            }
            else None
        }
        .toVector
        
        log.info( "Number of possible mid points: " + candidateDestinations.size )
            
        // Have several attempts at destinations before finally giving up
        val possibleMidPoints = midNodeOption match
        {
            case Some(mn)   => (0 until 20).iterator.map( _ => startPointAnnotationMap(mn.nodeId) )
            case None       =>
            {
                (0 until 100).iterator.map
                { _ =>
                
                    // Choose randomly from the top 25% by cost to get a mid-point
                    if ( !candidateDestinations.isEmpty )
                    {
                        val elementIndex = random.nextInt( candidateDestinations.size )
                        
                        Some( candidateDestinations(elementIndex)._2 )
                    }
                    else None
                }.flatten
            }
        }
        
        val possibleRoutes = possibleMidPoints.map
        { midPoint =>
        
            log.info( "Computing distances from second node" )
            val midPointAnnotationMap = runDijkstra( midPoint.node, targetDist )
            
            log.info( "Computing possible quarterpoints" )
            val possibleQuarterPoints = midPointAnnotationMap
                // Add distance annotation from the start node
                .filter { case (nid, annot) => startPointAnnotationMap contains nid }
                .map { case (nid, annot) => (nid, startPointAnnotationMap(nid), annot ) }
                .filter
                { case (nid, annot1, annot2) =>
                    
                    // Must be within threshold distance as a feasible midpoint
                    val withinDistance = (annot1.dist + annot2.dist) < 0.8 * targetDist
                    
                    // Must not share the same parent in both graphs, otherwise the path in
                    // will be the same as the path out. This is a rather aggressive filter
                    val retraceFootsteps =
                    {
                        (annot1.parent, annot2.parent) match
                        {
                            case (Some(p1), Some(p2))   => p1.ra.node == p2.ra.node
                            case _                      => false
                        }
                    }
                    
                    withinDistance && !retraceFootsteps
                }
                .toSeq
                .sortBy { case (nid, annot1, annot2) => (annot1.cost + annot2.cost)/(annot1.dist + annot2.dist) }
                .toSeq
                    
            val trimmedQuarterPoints = possibleQuarterPoints
                .take( possibleQuarterPoints.size / 4 )
                .toIndexedSeq
                
            
       
            log.info( "Number of quarter-points: " + trimmedQuarterPoints.size )
                
            log.info( "Generating sample quarter-point pairs." )
            val possibleQuarterPointPairs = (0 until 100).iterator.map
            { i =>
            
                val startPoint = trimmedQuarterPoints( random.nextInt( trimmedQuarterPoints.size ) )
                
                val combinations : Iterator[QuarterPointPair] = (0 until 200).iterator.map
                { j =>
                    val endPoint = trimmedQuarterPoints( random.nextInt( trimmedQuarterPoints.size ) )
                    
                    (startPoint, endPoint)
                }
                
                
                combinations.filter
                { case ((nid1, annot11, annot12), (nid2, annot21, annot22)) => 
                
                    val routeDist = annot11.dist + annot12.dist + annot21.dist + annot22.dist
                    
                    ( (nid1 != nid2) && (routeDist > (targetDist * 0.8)) && (routeDist < (targetDist * 1.2)) )
                }
                .take( 50 )
            }
            .flatten
            .toSeq
            
            log.info( "Number of possible quarter-point pairs: " + possibleQuarterPointPairs.size )
            
            (midPoint, midPointAnnotationMap, possibleQuarterPointPairs)
      
            
            log.info( "Evaluating sample quarter-point pairs." )
            var quarterPointPairCount = 0
            val costedQuarterPointPairs = possibleQuarterPointPairs.map
            { case ((nid1, annot11, annot12), (nid2, annot21, annot22)) => 
            
                val routeNodeIds = routePath( startPointAnnotationMap, midPointAnnotationMap, nid1, nid2, trimEnd ).map( _.ra.node.nodeId ).toSeq
                     
                val circularityRatio = routeNodeIds.toSet.size.toDouble / routeNodeIds.size.toDouble
                
                val cost = annot11.cost + annot12.cost + annot21.cost + annot22.cost
                val routeDist = annot11.dist + annot12.dist + annot21.dist + annot22.dist
                
                quarterPointPairCount += 1
                
                // Upweight routes where nid1 and nid2 are farther apart
                (nid1, nid2, cost, circularityRatio, routeDist, annot11, annot12, annot21, annot22)
            }
            .filter { _._4 > 0.70 }
            .take( 500 )
            .toVector
            .sortBy( x => (x._3/x._5) / x._4 )
            .toVector
            
            log.info( "Possible quarter-point pairs: " + costedQuarterPointPairs.size )
            val topHalf = costedQuarterPointPairs.take( costedQuarterPointPairs.size / 2 )
            
            val debugPoints =
                //candidateDestinations.map( x => DebugPoint(x._2.node.coord, "M") ).toSeq// ++
                possibleQuarterPoints.map( x => DebugPoint(x._2.node.coord, "Q") ).toSeq :+ DebugPoint(midPoint.node.coord, "M") :+ DebugPoint(startNode.coord, "S") 
                
            if ( !topHalf.isEmpty )
            {
                val chosenPairIndex = random.nextInt( costedQuarterPointPairs.size / 2 )

                // Find the best pair by cumulative cost
                val (bestId1, bestId2, cost, circularityRatio, routeDist, annot11, annot12, annot21, annot22) = costedQuarterPointPairs(chosenPairIndex)
                
                log.info( "Route distance: %.2f, route cost: %.2f".format( routeDist / 1000.0, cost / 1000.0 ) )
                
                // Now the route is:
                // * startNode -> quarterPoint -> annot1 -> threeQuarterPoint -> startNode. Enumerate
                // the coordinates on the way.
                
                val quarterPoint = startPointAnnotationMap(bestId1)
                val threeQuarterPoint = startPointAnnotationMap(bestId2)
                log.info( startNode.coord.lat + ", " + startNode.coord.lon )
                log.info( midPoint.node.coord.lat + ", " + midPoint.node.coord.lon )
                log.info( quarterPoint.node.coord.lat + ", " + quarterPoint.node.coord.lon )
                log.info( threeQuarterPoint.node.coord.lat + ", " + threeQuarterPoint.node.coord.lon )
                
                val fullRoute = routePath( startPointAnnotationMap, midPointAnnotationMap, bestId1, bestId2, trimEnd )
                /*val fullRoute = aStarRoutePath( Seq(
                    startNode,
                    quarterPoint.node,
                    midPoint.node,
                    threeQuarterPoint.node,
                    startNode ) )*/
                
                Some( FoundRoute( fullRoute, debugPoints ) )
            }
            else None
        }
        
        // Lazily find the first non-zero route
        val resIt = possibleRoutes.flatten
        if ( resIt.hasNext )
        {
            Some(resIt.next)
        }
        else None
    }
    
    def buildRoute( startNode : RouteNode, midNodeOption : Option[RouteNode], targetDist : Double ) : Option[RouteResult] =
    {
        findRoute( startNode, midNodeOption, targetDist ).map
        { fullRoute =>
        
            var lastEdgeName = ""
            var cumulativeDistance = 0.0
            var cumulativeAscent = 0.0
            
            var lastEdge : Option[EdgeAndBearing] = None
            
            val truncatedRoute = mutable.ArrayBuffer[RouteDirections]()
            val recentNodeDists = mutable.ArrayBuffer[NodeAndDistance]()
            val recentPics = mutable.Set[ScenicPoint]()
            val recentPOIs = mutable.Set[POI]()
            
            val seenPics = mutable.Set[ScenicPoint]()
            val seenPOIs = mutable.Set[POI]()
            
            // TODO: This is all horribly imperative. Refactor
            var lastNodeOption : Option[Node] = None
            fullRoute.path.zipWithIndex.foreach
            { case (pathEl, i) =>
            
                val destAnnotNode = pathEl.ra
                val inboundEdge = pathEl.re
                
                
                inboundEdge match
                {
                    case None       => ("Start", Seq())
                    case Some(eb)    =>
                    {
                        val e = eb.edge

                        pathEl.edgeNodes.foreach
                        { n =>
                            lastNodeOption match
                            {
                                case Some( lastNode ) =>
                                {
                                    val heightDelta = n.height - lastNode.height
                                    if ( heightDelta > 0.0 ) cumulativeAscent += heightDelta
                                    cumulativeDistance += n.coord.distFrom( lastNode.coord )
                                }
                                case _ =>
                            }
                            recentNodeDists.append( NodeAndDistance( n, cumulativeDistance ) )
                            lastNodeOption = Some(n)
                        }
                        
                        recentPics ++= e.scenicPoints.filter( p => !seenPics.contains(p) )
                        recentPOIs ++= e.pois.map(_.poi).filter( p => !seenPOIs.contains(p) )
                        seenPics ++= e.scenicPoints
                        seenPOIs ++= e.pois.map(_.poi)
                        
                        val (bearingDelta, lastName) = lastEdge match
                        {
                            case Some(leb)  =>
                            {
                                val le = leb.edge
                                (normaliseDegrees( eb.bearing - leb.bearing ).toFloat, le.name)
                            }
                            case None       => (0.0f, "")
                        }
                                
                        if ( lastName != e.name )
                        {
                            truncatedRoute.append( new RouteDirections( recentNodeDists.toArray, recentPics.toArray, recentPOIs.toArray, e.name, e.dist / 1000.0, cumulativeDistance / 1000.0, destAnnotNode.node.height, bearingDelta, recentNodeDists.last.node.coord ) )
                            recentNodeDists.clear()
                            recentPics.clear()
                            recentPOIs.clear()
                        }
                    }
                }
                lastEdge = inboundEdge
            }

            new RouteResult(
                truncatedRoute.toArray,
                cumulativeDistance / 1000.0,
                cumulativeAscent,
                fullRoute.debugPoints.toArray )
        }
    }
}





