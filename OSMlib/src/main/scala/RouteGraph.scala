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

case class RouteNode( val nodeId : Int, val node : Node, val landCoverScore : Float )
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
    val landCoverScore : Float,
    val nodes : Array[Node] )
{
}

case class EdgeAndBearing( val edge : RouteEdge, val bearing : Float )
{
    //def invertBearing = EdgeAndBearing( edge, normaliseDegrees( bearing - 180.0 ).toFloat )
}

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

case class ForwardPathElement( val routeAnnotation : RouteAnnotation, val outgoingEB : Option[EdgeAndBearing] )
{
    def edgeNodes : Seq[Node] =
    {
        outgoingEB match
        {
            case None => Seq()
            case Some( eab ) =>
            {
                val rawNodes = eab.edge.nodes
                if ( rawNodes.head == routeAnnotation.routeNode.node ) rawNodes
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
	val outboundNodes : Array[NodeAndDistance], 
	val outboundPics : Array[ScenicPoint],
	val outboundPOIs : Array[POI],
	val edgeName : String,
	val cumulativeDistance : Double,
	val cumulativeTime : Double,
	val elevation : Double,
	val bearing : Float,
	val coord : Coord,
	val directionsText : String )

case class DebugPoint( coord : Coord, name : String, title : String )

case class RouteResult(
    directions : Array[RouteDirections],
    distance : Double,
    time : Double,
    ascent : Double,
    debugPoints : Array[DebugPoint] )



case class RouteSegment( val edgeName : String, val pathElements : Seq[PathElement], val bearing : Float )


case class DebugWay( coords : Array[Coord], score : Double, scenicScore : Double )
case class DebugDest( coord : Coord, score : Double, title : String )
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
    
    def traceBack( inputEdgeOption : Option[EdgeAndBearing], endNode : RouteAnnotation, reverse : Boolean ) : Seq[PathElement] =
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
        
        
        assert( nodes.size == edgesBearings.size + 1 )
        nodes.zip( inputEdgeOption +: edgesBearings.map( e => Some(e) ) ).map { case (n, e) => PathElement( n, e ) }
    }
    
    def shortestPathTo( endNode : RouteAnnotation ) : Seq[ForwardPathElement] =
    {
        var iterNode : Option[PathElement] = Some( PathElement(endNode, None) )
        
        var revPath = mutable.ArrayBuffer[ForwardPathElement]()
        
        do
        {
            val PathElement(ra, edgeAndBearing) = iterNode.get
            
            revPath.append( ForwardPathElement( ra, edgeAndBearing/*.map( _.invertBearing )*/ ) )
            
            iterNode = ra.parent
        }
        while ( iterNode != None )
        
        val path = revPath.reverse.toSeq
        
        path.dropRight(1).foreach
        { p =>
        
            assert( !p.outgoingEB.isEmpty )
        }
        
        assert( path.last.outgoingEB.isEmpty )
        
        path
    }
    
    private def runDFS(
    	routeType : RouteType,
    	startNode : RouteNode,
        queueOrdering : Ordering[RouteAnnotation],
        maxDist : Double,
        endNode : Option[RouteNode],
        temporaryEdgeWeights : Map[Int, Double],
        // TODO: This is a horrible hack. Compute bearings elsewhere
        computeBearings : Boolean = false ) : AnnotationMap =
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
                            
                            val bearing = if ( computeBearings ) minEl.routeNode.coord.bearing( node.coord ).toFloat else 0.0f
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
                val r1dist = ra1.cumulativeCost + ra1.routeNode.coord.approxDistFrom( endNode.coord )
                val r2dist = ra2.cumulativeCost + ra2.routeNode.coord.approxDistFrom( endNode.coord )
                if ( r1dist != r2dist ) r1dist < r2dist
                else ra1.routeNode.nodeId < ra2.routeNode.nodeId
            } ),
            Double.MaxValue,
            Some(endNode),
            temporaryEdgeWeights,
            computeBearings=true)
    }
        
    private def aStarShortestPath( routeType : RouteType, startNode : RouteNode, endNode : RouteNode, temporaryEdgeWeights : Map[Int, Double] ) : Seq[ForwardPathElement] =
    {
        val annot = runAStar( routeType, startNode, endNode, temporaryEdgeWeights )
            
        val endNodeRA = annot(endNode.nodeId)
        val path = shortestPathTo( endNodeRA )
        
        assert( path.head.routeAnnotation.routeNode.nodeId == startNode.nodeId )
        assert( path.last.routeAnnotation.routeNode.nodeId == endNode.nodeId )
        
        path
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
    
    // Distance is in metres
    private def spacingRatio( dist : Double ) = (dist / 30.0) max 300.0
    
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
            	Some( DebugDest( an.routeNode.coord, cost, an.routeNode.landCoverScore.toString ) )
            }
            else None
        }
        .toArray
        
        log.info( "Debug data computed" )
        DebugData( allWays, cds, pois, scenicPoints )
    }
    
    // *************** The main mechanics of route finding happens here ***************
    
    case class FoundRoute( val path : Seq[ForwardPathElement], val debugPoints : Seq[DebugPoint] )
    
    private def filterDestinations( routeType : RouteType, annotations : Seq[RouteAnnotation],  minSpacing : Double ) : Seq[(Double, RouteAnnotation)] =
    {
        val csm = new CoordSpacingManager( minSpacing )
        
        // Filter out points which have only one destination, then order by the nice-ness of
        // the adjacent edges 
        val orderedPoints = annotations
            .filter
            { annot =>
            
                val dests = routeType.validDests( annot.routeNode )
                dests.size > 2
            }
            .map
            { annot =>
            
                val dests = routeType.validDests( annot.routeNode )
                val cost = dests.map( de => routeType.score(de.edge).value ).min
                
                (cost * annot.routeNode.landCoverScore, annot)
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
        val fillInPoints = orderedPoints
	        .flatMap
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
            .filter { case (nid, annot) => annot.cumulativeDistance > targetDist * 0.1 && annot.cumulativeDistance < targetDist * 0.3 }
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
                
                /*
                val bearings = Seq(
                    startNode.coord.bearing(midPoint.routeNode.coord),
                    midPoint.routeNode.coord.bearing(annot.routeNode.coord),
                    annot.routeNode.coord.bearing(startNode.coord) )
                    .map( Math.abs _ )
                    
                val minBearing = bearings.min
                val maxBearing = bearings.max
                
                
                // Equilateral is best in terms of routes (so smaller is better) 
                val shapeCost = maxBearing / minBearing
                
                log.info( "%.2f (%s)".format( shapeCost, bearings.mkString(",")) )*/
                    
                // Within distance and filter out very squashed triangles
                withinDistance //&& (shapeCost < 2.0)
            }
            .toIndexedSeq
            
            log.info( "Possible quarter points: " + possibleQuarterPoints.size )
           
            
            val possibleRoutes = (0 until 5).map
            { _ =>
                
                val qp1 = chooseDest( possibleQuarterPoints )._2.routeNode
                
                var alreadySeenUpweight = immutable.Map[Int, Double]()
                val section1 = aStarShortestPath( routeType, startNode, qp1, alreadySeenUpweight )
                section1.flatMap( _.outgoingEB ).foreach( e => alreadySeenUpweight += (e.edge.edgeId -> 4.0) )
                val dist1 = section1.last.routeAnnotation.cumulativeDistance
                
                val section2 = aStarShortestPath( routeType, qp1, midPoint.routeNode, alreadySeenUpweight )
                section2.flatMap( _.outgoingEB ).foreach( e => alreadySeenUpweight += (e.edge.edgeId -> 4.0) )
                val dist2 = section2.last.routeAnnotation.cumulativeDistance
                
                val section3 = aStarShortestPath( routeType, midPoint.routeNode, startNode, alreadySeenUpweight )
                
                val totalDist = section1.last.routeAnnotation.cumulativeDistance + section2.last.routeAnnotation.cumulativeDistance + section3.last.routeAnnotation.cumulativeDistance
                val totalCost = section1.last.routeAnnotation.cumulativeCost + section2.last.routeAnnotation.cumulativeCost + section3.last.routeAnnotation.cumulativeCost
                
                log.info( "Possible distance: " + totalDist )
                
                val dps = Seq(
                    DebugPoint(midPoint.routeNode.coord, "blue_MarkerM", ""),
                    DebugPoint(startNode.coord, "blue_MarkerS", ""),
                    DebugPoint(qp1.coord, "blue_MarkerQ", "") )
                    
                
                
                // Smaller is better
                val costRatio = (totalCost / totalDist)
                
                (totalDist, costRatio,
                    // Drop the last element of partial sections as they will be the final node with
                    // no outgoing edge
                    section1.dropRight(1) ++
                    section2.dropRight(1) ++
                    section3, dps)
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
        
        
            val peSegments = mutable.ArrayBuffer[mutable.ArrayBuffer[ForwardPathElement]]()
            fullRoute.path.foreach
            { pe =>
            
                if ( peSegments.isEmpty )
                {
                    peSegments.append( mutable.ArrayBuffer[ForwardPathElement]() )
                }
                else
                {
                    val thisNameOption = pe.outgoingEB.map( _.edge.name )
                    val numDests = routeType.validDests(pe.routeAnnotation.routeNode).size
                    
                    val lastNameOption = peSegments.last.last.outgoingEB.map( _.edge.name )
                    
                    // Start a new segment if the road name changes 
                    if ( /*numDests > 2 &&*/ thisNameOption != lastNameOption )
                    {
                        peSegments.append( mutable.ArrayBuffer[ForwardPathElement]() )
                    }
                }
                peSegments.last.append( pe )
            }
        
            var cumulativeDistance = 0.0
            var cumulativeTime = 0.0
            var cumulativeAscent = 0.0
            
            var lastPEO : Option[ForwardPathElement] = None
            
            val truncatedRoute = mutable.ArrayBuffer[RouteDirections]()
            
            val seenPics = mutable.Set[ScenicPoint]()
            val seenPOIs = mutable.Set[POI]()
            
            // TODO: This is all horribly imperative. Refactor
            var lastNodeOption : Option[Node] = None
            var size = fullRoute.path.size
            
            peSegments.map
            { peSeg =>
            
                val startNode = peSeg.head.routeAnnotation
                
                if ( peSeg.head.outgoingEB.isDefined )
                {
                    val startEB = peSeg.head.outgoingEB.get
                    
                    val lastEdge = lastPEO.flatMap( _.outgoingEB )
                    val bearingDelta = lastEdge match
                    {
                        case Some(leb)  =>
                        {
                            val le = leb.edge
                            normaliseDegrees( startEB.bearing - leb.bearing ).toFloat
                        }
                        case None       => 0.0f
                    }
                    
                    val outboundNodeDists = mutable.ArrayBuffer[NodeAndDistance]()
                    val outboundPics = mutable.Set[ScenicPoint]()
                    val outboundPOIs = mutable.Set[POI]()
                    peSeg.foreach
                    { fpe =>
                    
                        fpe.edgeNodes.foreach
                        { n =>
                        
                            lastNodeOption.foreach
                            { ln =>
                            
                                val distDelta = ln.coord.distFrom(n.coord)
                                cumulativeDistance += distDelta
                                cumulativeTime += routeType.speed( fpe.outgoingEB.get.edge ).timeToCover(distDelta)
                                
                                outboundNodeDists.append( NodeAndDistance( n, cumulativeDistance) )
                            }
                        
                            lastNodeOption = Some(n)
                        }
                        
                        fpe.outgoingEB.foreach
                        { eb =>
                        
                            val e = eb.edge
                            outboundPics ++= e.scenicPoints.filter( p => !seenPics.contains(p) )
                            outboundPOIs ++= e.pois.map(_.poi).filter( p => !seenPOIs.contains(p) )
                            seenPics ++= e.scenicPoints
                            seenPOIs ++= e.pois.map(_.poi)
                        }
                    }
                    
                    val routeText =
                    {
                        val prefix = if ( bearingDelta < -120.0 ) "Sharp left onto"
                        else if ( bearingDelta < -45.0 ) "Left onto"
                        else if ( bearingDelta < -10.0 ) "Slight left onto"
                        else if ( bearingDelta > 120.0 ) "Sharp right onto"
                        else if ( bearingDelta > 45.0 ) "Right onto"
                        else if ( bearingDelta > 10.0 ) "Slight right onto"
                        else "Continue on"
                        
                        prefix + " " + startEB.edge.name
                    }
                    
                    truncatedRoute.append( new RouteDirections(
                        outboundNodes = outboundNodeDists.toArray,
                        outboundPics = outboundPics.toArray,
                        outboundPOIs = outboundPOIs.toArray,
                        edgeName = startEB.edge.name,
                        cumulativeDistance = cumulativeDistance / 1000.0,
                        cumulativeTime = cumulativeTime,
                        elevation = startNode.routeNode.height,
                        bearing = bearingDelta,
                        coord = startNode.routeNode.coord,
                        directionsText = routeText ) )
                }   
                        
                lastPEO = Some(peSeg.last)
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





