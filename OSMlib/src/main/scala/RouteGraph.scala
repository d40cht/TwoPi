package org.seacourt.osm.route

import org.seacourt.osm._

import scala.collection.{mutable, immutable}


// Icons for POIs: http://www.sjjb.co.uk/mapicons/contactsheet#tourist

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
    // From dbpedia dumps, cross-link on name and coords via: geo_coordinates_en.nt.bz2
    val description : Option[String],
    // From images_en.nt.bz2
    val imageUrl : Option[String],
    // From dbpedia link
    val url : Option[String]
)

case class ScenicPoint( coord : Coord, score : Double, picIndex : Int )
{
    assert( score >= 0.0 && score <= 1.0 )
}

case class RouteNode( val nodeId : Int, val coord : Coord, val height : Float )
{
    val destinations = mutable.ArrayBuffer[(RouteNode, RouteEdge)]()
    
    def addEdge( dest : RouteNode, edge : RouteEdge ) =
    {
        destinations.append( (dest, edge) )
    }
}

// TODO: There should really be a height delta on RouteEdge to get the costs right for long routes.
// but then we'd need to know which way we were going - so instate when doing one-way logic.
case class RouteEdge( val dist : Double, val cost : Double, val nameId : Int, val scenicPoints : Array[ScenicPoint] )


case class EdgeAndBearing( val edge : RouteEdge, val bearing : Float )

case class PathElement( ra : RouteAnnotation, re : Option[EdgeAndBearing] )

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

case class RouteDirections( val inboundPics : List[ScenicPoint], val edgeName : String, val dist : Double, val cumulativeDistance : Double, val elevation : Double, bearing : Float )

class RoutableGraph( val strings : Array[String], val nodes : Array[RouteNode], val scenicPoints : Array[ScenicPoint] ) extends Logging
{
    val treeMap = new RTreeIndex[RouteNode]()
    
    log.info( "Populating route node tree map for quick indexing.")
    nodes.foreach( n => treeMap.add( n.coord, n ) )
    log.info( "... complete." )
    
    def getClosest( coord : Coord ) : RouteNode = treeMap.nearest( coord ).get

    type AnnotationMap = mutable.HashMap[Int, RouteAnnotation]

    private def runDijkstra( startNode : RouteNode, maxDist : Double, random : util.Random ) : AnnotationMap =
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
                        
                        val bearing = minEl.node.coord.bearing( node.coord ).toFloat
                        nodeAnnot.parent = Some( PathElement(minEl, Some(EdgeAndBearing(edge, bearing))) )
                        
                        q += nodeAnnot
                    }
                }
            }
        }
        
        annotations
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
        def q( v : Double ) = ((v * 200.0).toInt).toDouble / 200.0
        
        new Coord( lon = q(c.lon), lat = q(c.lat) )
    }
    

    private def routePath( startPointAnnotationMap : AnnotationMap, midPointAnnotationMap : AnnotationMap, id1 : Int, id2 : Int, pruneDistalOverlap : Boolean ) : Seq[PathElement] =
    {
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
    
    type QuarterPoint = (Int, RouteAnnotation, RouteAnnotation)
    type QuarterPointPair = (QuarterPoint, QuarterPoint)
    
    
    // *************** The main mechanics of route finding happens here ***************
    
    private def findRoute( startNode : RouteNode, targetDist : Double) : Option[Seq[PathElement]] =
    {
        val random = util.Random
        val startPointAnnotationMap = runDijkstra( startNode, targetDist, random )
        
        log.info( "Computing distances from start node" )

        val allDestinationsRaw = startPointAnnotationMap
            .filter { case (nid, annot) => annot.dist > targetDist * 0.1 && annot.dist < targetDist * 0.5 }
            .toSeq
            .sortBy { case (nid, annot) => annot.cost/annot.dist }
            
        // Group destinations by quantised coordinates in order to ameliorate the bias towards
        // locations that have more ways per unit area.
        val allDestinations = allDestinationsRaw
            .groupBy { case (n, annot) => quantiseCoord( annot.node.coord ) }
            .map { case (c, allPoints) => allPoints.sortBy( p => p._2.cost / p._2.dist ).head }
            
        val candidateDestinations = allDestinations
                .take( allDestinations.size / 4 )
                .toVector
            
        // Have several attempts at destinations before finally giving up
        val possibleRoutes = (0 until 20).iterator.map
        { _ =>
            // Choose randomly from the top 25% by cost to get a mid-point
            if ( !candidateDestinations.isEmpty )
            {
                val elementIndex = random.nextInt( candidateDestinations.size )
                
                val midPoint = candidateDestinations(elementIndex)._2
                
                log.info( "Computing distances from second node" )
                val midPointAnnotationMap = runDijkstra( midPoint.node, targetDist, random )
                
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
                        // will be the same as the path out
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
                    .take( possibleQuarterPoints.size /4 )
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
                
                    val routeNodeIds = routePath( startPointAnnotationMap, midPointAnnotationMap, nid1, nid2, false ).map( _.ra.node.nodeId ).toSeq
                         
                    val circularityRatio = routeNodeIds.toSet.size.toDouble / routeNodeIds.size.toDouble
                    
                    val cost = annot11.cost + annot12.cost + annot21.cost + annot22.cost
                    val routeDist = annot11.dist + annot12.dist + annot21.dist + annot22.dist
                    
                    quarterPointPairCount += 1
                    
                    // Upweight routes where nid1 and nid2 are farther apart
                    (nid1, nid2, cost, circularityRatio, routeDist, annot11, annot12, annot21, annot22)
                }
                .filter { _._4 > 0.70 }
                .take( 50 )
                .toVector
                .sortBy( x => (x._3/x._5) / x._4 )
                .toVector
                
                log.info( "Possible quarter-point pairs: " + costedQuarterPointPairs.size )
                val topHalf = costedQuarterPointPairs.take( costedQuarterPointPairs.size / 2 )
                
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
                    
                    val fullRoute = routePath( startPointAnnotationMap, midPointAnnotationMap, bestId1, bestId2, true )
                    
                    Some(fullRoute)
                }
                else None
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
    
    def buildRoute( startNode : RouteNode, targetDist : Double) : Option[RouteResult] =
    {
        findRoute( startNode, targetDist ).map
        { fullRoute =>
        
            val nodeList = fullRoute.map( _.ra.node ).toList
            val edgeList = fullRoute.flatMap( _.re ).toList
            
            val topPics = edgeList
                .flatMap( _.edge.scenicPoints )
                .distinct
                .sortBy( -_.score )
                .take(10)
                .toSet

            val scenicPointEdgePairsByDist : Seq[(ScenicPoint, RouteEdge, Double)] = fullRoute
                .map
                { case PathElement(raNode, edgeBearing) =>
                    
                    edgeBearing.map
                    { eb =>
                        eb.edge
                            .scenicPoints
                            .filter( sp => topPics.contains(sp) )
                            .map
                            { sp =>
                                (sp, eb.edge, sp.coord.distFrom( raNode.node.coord ) )
                            }
                    }
                }
                .flatten.flatten
                
            val topPicsByEdge = scenicPointEdgePairsByDist
                .groupBy( _._1 )
                .map
                { case (sp, options) =>
                
                    val topOption = options.sortBy( _._3 ).head
                    (sp, topOption._2)
                }
                .toSet
            
            
            var lastEdgeName = ""
            var dist = 0.0
            
            var lastEdge : Option[EdgeAndBearing] = None
            
            val truncatedRoute = mutable.ArrayBuffer[RouteDirections]()
            val recentPics = mutable.Set[ScenicPoint]()
            
            fullRoute.zipWithIndex.map
            { case (pathEl, i) =>
            
                val destAnnotNode = pathEl.ra
                val inboundEdge = pathEl.re
                
                
                inboundEdge match
                {
                    case None       => ("Start", Seq())
                    case Some(eb)    =>
                    {
                        val e = eb.edge

                        dist += e.dist / 1000.0
                        recentPics ++= e.scenicPoints.filter( sp => topPicsByEdge.contains( (sp, e) ) )
                        
                        val (bearingDelta, lastNameId) = lastEdge match
                        {
                            case Some(leb)  =>
                            {
                                val le = leb.edge
                                (normaliseDegrees( eb.bearing - leb.bearing ).toFloat, le.nameId)
                            }
                            case None       => (0.0f, "")
                        }
                                
                        if ( lastNameId != e.nameId )
                        {
                            truncatedRoute.append( new RouteDirections( recentPics.toList, strings(e.nameId), e.dist / 1000.0, dist, destAnnotNode.node.height, bearingDelta ) )
                            recentPics.clear()
                        }
                    }
                }
                lastEdge = inboundEdge
            }
            
             
            for ( rd <- truncatedRoute )
            {
                println( "[% 5.2fkm] % 3.2fkm, % 3.0fm elevation: %s, bearing: % 4d [%d]".format( rd.cumulativeDistance, rd.dist, rd.elevation, rd.edgeName, rd.bearing.toInt, rd.inboundPics.size ) )
            }
            
            new RouteResult( nodeList, truncatedRoute.flatMap( _.inboundPics ).distinct.toSeq )
        }
    }
}


