package org.seacourt.osm.route

import org.seacourt.osm._
import org.seacourt.osm.poi.POITypes._

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

case class EdgeDest( val node : RouteNode, val edge : RouteEdge, val oneWayViolation : Boolean )
{
}

case class RouteNode( val nodeId : Int, val node : Node )
{
    def coord = node.coord
    def height = node.height
    
    val destinations = mutable.ArrayBuffer[EdgeDest]()
    
    def addEdge( dest : RouteNode, edge : RouteEdge, oneWayViolation : Boolean ) =
    {
        destinations.append( EdgeDest(dest, edge, oneWayViolation) )
    }
}



// TODO: There should really be a height delta on RouteEdge to get the costs right for long routes.
// but then we'd need to know which way we were going - so instate when doing one-way logic.

case class NearbyPOI( val dist : Float, val poi : POI )
{
}



object RouteEdge
{
    def name( tagMap : Map[String, String] ) : String =
    {
    	val highwayAnnotation : Option[String] = tagMap.get("highway")
        val junctionAnnotation : Option[String] = tagMap.get("junction")
        val bridgeAnnotation : Option[String] = tagMap.get("bridge")
        val nameAnnotation : Option[String] = tagMap.get("name")
        val refAnnotation : Option[String] = tagMap.get("ref")
        val footAnnotation : Option[String] = tagMap.get("foot")
        
        if ( !nameAnnotation.isEmpty ) nameAnnotation.get
        else if ( !refAnnotation.isEmpty ) refAnnotation.get
        else if ( !junctionAnnotation.isEmpty ) junctionAnnotation.get
        else if ( !bridgeAnnotation.isEmpty ) "bridge"
        else "Unnamed " + highwayAnnotation.get
    }
}

case class RouteEdge(
    val wayTags : Map[String, String], 
    val edgeId : Int,
    val dist : Double,
    val absHeightDelta : Double,
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
                if ( rawNodes.last == ra.routeNode.node ) rawNodes
                else rawNodes.reverse
            }
        }
    }
}

case class RouteAnnotation( val routeNode : RouteNode, var cumulativeCost : Double, var cumulativeDistance : Double )
{
    var parent : Option[PathElement]    = None
}

case class NodeAndDistance( val node : Node, val distance : Double )

case class RouteDirections(
	val inboundNodes : Array[NodeAndDistance], 
	val inboundPics : Array[ScenicPoint],
	val inboundPOIs : Array[POI],
	val edgeName : String,
	val dist : Double,
	val cumulativeDistance : Double,
	val cumulativeTime : Double,
	val elevation : Double,
	bearing : Float,
	coord : Coord )

case class DebugPoint( coord : Coord, name : String, title : String )

case class RouteResult(
    directions : Array[RouteDirections],
    distance : Double,
    time : Double,
    ascent : Double,
    debugPoints : Array[DebugPoint] )



case class RouteSegment( val edgeName : String, val pathElements : Seq[PathElement], val bearing : Float )


case class DebugWay( coords : Array[Coord], score : Double, scenicScore : Double )
case class DebugDest( coord : Coord, score : Double )
case class DebugData( ways : Array[DebugWay], dests : Array[DebugDest], pois : Array[POI], scenicPoints : Array[ScenicPoint] )

class RoutableGraph( val nodes : Array[RouteNode], val scenicPoints : Array[ScenicPoint] ) extends Logging
{
    val treeMap = new RTreeIndex[RouteNode]()
    
    log.info( "Populating route node tree map for quick indexing.")
    nodes.foreach( n => treeMap.add( n.coord, n ) )
    log.info( "... complete." )
    
    def getClosest( routeType : RouteType, coord : Coord ) : RouteNode =
    {
        val all = treeMap.nearest( coord, 50 )
        
        val valid = all.filter
        { rn =>
        
            rn.destinations.exists( ed => !routeType.score(ed.edge).isZero )
        }

        valid.head
    }

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
    
    private def runDFS(
    	routeType : RouteType,
    	startNode : RouteNode,
        queueOrdering : Ordering[RouteAnnotation],
        maxDist : Double,
        endNode : Option[RouteNode],
        temporaryEdgeWeights : Map[Int, Double] ) : AnnotationMap =
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
                
                if ( Some(minEl.routeNode) == endNode ) break
                
                visited.add(minEl.routeNode.nodeId)
                
                minEl.routeNode.destinations.foreach
                { case EdgeDest(node, edge, oneWayViolation) =>
                    
                    if ( !(oneWayViolation && routeType.respectOneWay) && !visited.contains(node.nodeId) )
                    {
                        val nodeAnnot = annotations.getOrElseUpdate( node.nodeId, RouteAnnotation( node, Double.MaxValue, Double.MaxValue ) )
                        val thisCost = minEl.cumulativeCost + edge.dist / routeType.score(edge).value * temporaryEdgeWeights.getOrElse( edge.edgeId, 1.0 )
                        val thisDist = minEl.cumulativeDistance + edge.dist
                        
                        if ( nodeAnnot.cumulativeCost > thisCost && thisDist < maxDist )
                        {
                            q -= nodeAnnot
                            
                            nodeAnnot.cumulativeCost = thisCost
                            nodeAnnot.cumulativeDistance = minEl.cumulativeDistance + edge.dist
                            
                            val bearing = minEl.routeNode.coord.bearing( node.coord ).toFloat
                            nodeAnnot.parent = Some( PathElement(minEl, Some(EdgeAndBearing(edge, bearing))) )
                            
                            q += nodeAnnot
                        }
                    }
                }
            }
        }
        
        annotations
    }
    
    private def runDijkstra( routeType : RouteType, startNode : RouteNode, maxDist : Double, temporaryEdgeWeights : Map[Int, Double] ) : AnnotationMap = runDFS(
        routeType,
        startNode,
        Ordering.fromLessThan( (ra1 : RouteAnnotation, ra2 : RouteAnnotation) =>
        {
            if ( ra1.cumulativeCost != ra2.cumulativeCost ) ra1.cumulativeCost < ra2.cumulativeCost
            else ra1.routeNode.nodeId < ra2.routeNode.nodeId
        } ),
        maxDist,
        None,
        temporaryEdgeWeights )
        
    private def runAStar( routeType : RouteType, startNode : RouteNode, endNode : RouteNode, temporaryEdgeWeights : Map[Int, Double] ) : AnnotationMap =
    {
        runDFS(
            routeType,
            startNode,
            Ordering.fromLessThan( (ra1 : RouteAnnotation, ra2 : RouteAnnotation) =>
            {
                val r1dist = ra1.cumulativeCost + ra1.routeNode.coord.distFrom( endNode.coord )
                val r2dist = ra2.cumulativeCost + ra2.routeNode.coord.distFrom( endNode.coord )
                if ( r1dist != r2dist ) r1dist < r2dist
                else ra1.routeNode.nodeId < ra2.routeNode.nodeId
            } ),
            Double.MaxValue,
            Some(endNode),
            temporaryEdgeWeights )
    }
        
    private def aStarShortestPath( routeType : RouteType, startNode : RouteNode, endNode : RouteNode, temporaryEdgeWeights : Map[Int, Double] ) : Seq[PathElement] =
    {
        val annot = runAStar( routeType, startNode, endNode, temporaryEdgeWeights )
            
        val endNodeRA = annot(endNode.nodeId)
        traceBack( endNodeRA, true )
    }

    
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
                .takeWhile { case ( pe1, pe2 ) => pe1.ra.routeNode.nodeId == pe2.ra.routeNode.nodeId }
                .size - 1
        }
        else 0
        
        seg1 ++ seg2.dropRight(distalOverlap) ++ seg3.drop(distalOverlap) ++ seg4
        
    }
    
    private def aStarRoutePath( routeType : RouteType, nodes : Seq[RouteNode] ) : Seq[PathElement] =
    {
        nodes.sliding(2).flatMap { case Seq( from, to ) => aStarShortestPath( routeType, from, to, Map() ) }.toSeq
    }
    
    type QuarterPoint = (Int, RouteAnnotation, RouteAnnotation)
    type QuarterPointPair = (QuarterPoint, QuarterPoint)
    
    private def isDestinationPoint( poi : POI ) : Boolean =
    {
		poi.poiType match
        {
            //case Cafe | Pub | SurveyPoint | Peak | Cave | Archaeological | Memorial | Ruins | Monument | Museum | Historic | Viewpoint | Place | Natural => true
            case SurveyPoint | Peak | Cave | Archaeological | Memorial | Ruins | Monument | Museum | Historic | Viewpoint | Place | Natural => true
            case _ => false
        }
    }
    
    private def spacingRatio( dist : Double ) = dist / 30.0
    
    private def atRandomProportionalSelect( options : immutable.IndexedSeq[Double] ) : Int =
    {
        val cum = options.map
        {
            var acc = 0.0;
            v =>
            {
                assert( v >= 0.0, "Value cannot be negative" )
                acc += v
                acc
            }
        }
        
        var max = cum.last
        
        val point = util.Random.nextDouble * max
        
        options.indexWhere( _ > point )
    }
    
    private def chooseDest( dests : immutable.IndexedSeq[(Double, RouteAnnotation)] ) =
    {
        val elementIndex = util.Random.nextInt( dests.size )
        dests(elementIndex)
    }
    
    def debugRoute( routeType : RouteType, startNode : RouteNode, targetDist : Double ) : DebugData =
    {
        val startPointAnnotationMap = runDijkstra( routeType, startNode, targetDist, Map() )
        
        val allEdges = startPointAnnotationMap.flatMap
        { case (id, an) =>
        
            an.routeNode.destinations.map( _.edge )
        }
        .toSeq
        .distinct
        
        log.info( "Computing distances from start node: " + startNode.coord + ", " + targetDist )

        val allDestinationsRaw = startPointAnnotationMap
            .filter { case (nid, annot) => annot.cumulativeDistance > 0.0 && annot.cumulativeDistance <= targetDist }
            .map( _._2 )
            .toSeq
            
        val candidateDestinations = filterDestinations( routeType, allDestinationsRaw, spacingRatio(targetDist) )
        
        val pois = allDestinationsRaw.flatMap
        { an =>
        
        	an.routeNode.destinations.flatMap( _.edge.pois.map( _.poi ).filter { isDestinationPoint _ } )
        }
        .distinct
        .toArray
        
        val scenicPoints = allDestinationsRaw.flatMap
        { an =>
        
        	an.routeNode.destinations.flatMap( _.edge.scenicPoints )
        }
        .distinct
        .toArray
        
        log.info( "Number of possible destination points: " + candidateDestinations.size )
        
        val allWays = allEdges.map( e => DebugWay( e.nodes.map( _.coord ).toArray, routeType.score( e ).value, routeType.scenicScore(e).value) )
        	.filter(e => e.score != 0.0 )
        	.toArray
        	
        val cds = candidateDestinations.flatMap
        { case (cost, an) =>
            
            if ( cost != 0.0 )
            {
            	Some( DebugDest( an.routeNode.coord, cost ) )
            }
            else None
        }
        .toArray
        
        log.info( "Debug data computed" )
        DebugData( allWays, cds, pois, scenicPoints )
    }
    
    // *************** The main mechanics of route finding happens here ***************
    
    case class FoundRoute( val path : Seq[PathElement], val debugPoints : Seq[DebugPoint] )
    
    private def filterDestinations( routeType : RouteType, annotations : Seq[RouteAnnotation],  minSpacing : Double ) : Seq[(Double, RouteAnnotation)] =
    {
        val csm = new CoordSpacingManager( minSpacing )
        
        // Filter out points which have only one destination, then order by the nice-ness of
        // the adjacent edges 
        val orderedPoints = annotations
            .filter
            { annot =>
            
                val dests = annot.routeNode.destinations.filter( de => !routeType.score(de.edge).isZero )
                dests.size > 2
            }
            .map
            { annot =>
            
                val dests = annot.routeNode.destinations.filter( de => !routeType.score(de.edge).isZero ).toSeq
                val cost = dests.map( de => routeType.score(de.edge).value ).max
                
                (cost, annot)
            }
            .sortBy( -_._1 )
        
        // Choose feature points first
        val featurePoints = orderedPoints.flatMap
        { case (cost, annot) =>
        
            val dests = annot.routeNode.destinations
            val isDestNode = dests.flatMap( _.edge.pois.map( _.poi ) ).exists( isDestinationPoint _  )
            
            if ( isDestNode && csm.valid(annot.routeNode.coord) )
            {
                Some( (cost, annot) )
            }
            else None
        }
        
        // Then choose fill-in points
        val fillInPoints = orderedPoints.flatMap
        { case (cost, annot) =>
        
            val dests = annot.routeNode.destinations
            
            if ( csm.valid(annot.routeNode.coord) )
            {
                Some( (cost, annot) )
            }
            else None
        }
        
        featurePoints ++ fillInPoints
    }
     
    private def findRoute( routeType : RouteType, startNode : RouteNode, midNodeOption : Option[RouteNode], targetDistHint : Double ) : Option[FoundRoute] =
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
        val startPointAnnotationMap = runDijkstra( routeType, startNode, targetDist, Map() )
        
        log.info( "Computing distances from start node: " + startNode.coord + ", " + targetDist )

        val allDestinationsRaw = startPointAnnotationMap
            .filter { case (nid, annot) => annot.cumulativeDistance > targetDist * 0.05 && annot.cumulativeDistance < targetDist * 0.4 }
            .map( _._2 )
            .toSeq
            
        
        val candidateDestinations = filterDestinations( routeType, allDestinationsRaw, spacingRatio(targetDist) ).toIndexedSeq
        
        log.info( "Number of possible destination points: " + candidateDestinations.size )
        
        
            
        // Have several attempts at destinations before finally giving up
        val possibleMidPoints = midNodeOption match
        {
            case Some(mn)   => Seq( startPointAnnotationMap(mn.nodeId) )
            case None       =>
            {
                (0 until 10).iterator.map
                { _ =>
                
                    // Choose randomly from the top 25% by cost to get a mid-point
                    if ( !candidateDestinations.isEmpty )
                    {
                        Some( chooseDest(candidateDestinations)._2 )
                    }
                    else None
                }.flatten
            }
        }
        
        val candidateDestinationIds = candidateDestinations.map( _._2.routeNode.nodeId ).toSet
        
        val possibleRoutes = possibleMidPoints.map
        { midPoint =>
        
        	val midPointDist = midPoint.cumulativeDistance
            log.info( "Computing distances from second node, dist: " + midPointDist )
            
            val midPointAnnotationMap = runDijkstra( routeType, midPoint.routeNode, targetDist, Map() )
            
            log.info( "Computing possible quarterpoints" )
            val possibleQuarterPoints = candidateDestinations.filter
            { case (cost, annot) =>
            
                val annot1 = midPointAnnotationMap(annot.routeNode.nodeId)
                val annot2 = startPointAnnotationMap(annot.routeNode.nodeId)
                
                val withinDistance = (annot1.cumulativeDistance + annot2.cumulativeDistance + midPointDist) < (targetDist*1.2)
                    
                withinDistance
            }
            .toIndexedSeq
            
            log.info( "Possible quarter points: " + possibleQuarterPoints.size )
           
            
            val possibleRoutes = (0 until 5).map
            { _ =>
                
                val qp1 = chooseDest( possibleQuarterPoints )._2.routeNode
                
                var alreadySeenUpweight = immutable.Map[Int, Double]()
                val section1 = aStarShortestPath( routeType, startNode, qp1, alreadySeenUpweight )
                section1.flatMap( _.re ).foreach( e => alreadySeenUpweight += (e.edge.edgeId -> 4.0) )
                val dist1 = section1.last.ra.cumulativeDistance
                
                val section2 = aStarShortestPath( routeType, qp1, midPoint.routeNode, alreadySeenUpweight )
                section2.flatMap( _.re ).foreach( e => alreadySeenUpweight += (e.edge.edgeId -> 4.0) )
                val dist2 = section2.last.ra.cumulativeDistance
                
                val section3 = aStarShortestPath( routeType, midPoint.routeNode, startNode, alreadySeenUpweight )
                
                val totalDist = section1.last.ra.cumulativeDistance + section2.last.ra.cumulativeDistance + section3.last.ra.cumulativeDistance
                val totalCost = section1.last.ra.cumulativeCost + section2.last.ra.cumulativeCost + section3.last.ra.cumulativeCost
                
                log.info( "Possible distance: " + totalDist )
                
                val dps = Seq(
                    DebugPoint(midPoint.routeNode.coord, "blue_MarkerM", ""),
                    DebugPoint(startNode.coord, "blue_MarkerS", ""),
                    DebugPoint(qp1.coord, "blue_MarkerQ", "") )
                
                (totalDist, totalCost / totalDist, section1 ++ section2 ++ section3, dps)
            }
            .filter
            { case (dist, costRatio, route, dps) => 
                
                (dist > (targetDist * 0.8)) && (dist < (targetDist * 1.2))
            }
            .sortBy( _._2 )
            .headOption
            
                
            possibleRoutes.map
            { case (dist, costRatio, route, debugPoints) =>
                
                FoundRoute( route, debugPoints )
                
            }
        }
        
        // Lazily find the first non-zero route
        val resIt = possibleRoutes.flatten
        if ( resIt.hasNext )
        {
            Some(resIt.next)
        }
        else None
    }
    
    def buildRoute( routeType : RouteType, startNode : RouteNode, midNodeOption : Option[RouteNode], targetDist : Double ) : Option[RouteResult] =
    {
        findRoute( routeType, startNode, midNodeOption, targetDist ).map
        { fullRoute =>
        
            var lastEdgeName = ""
            var cumulativeDistance = 0.0
            var cumulativeTime = 0.0
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
                                    val thisDist = n.coord.distFrom( lastNode.coord )
                                    cumulativeDistance += thisDist
                                    cumulativeTime += routeType.speed( e ).timeToCover(thisDist)
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
                            truncatedRoute.append( new RouteDirections( recentNodeDists.toArray, recentPics.toArray, recentPOIs.toArray, e.name, e.dist / 1000.0, cumulativeDistance / 1000.0, cumulativeTime, destAnnotNode.routeNode.height, bearingDelta, recentNodeDists.last.node.coord ) )
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
                cumulativeTime,
                cumulativeAscent,
                fullRoute.debugPoints.toArray )
        }
    }
}





