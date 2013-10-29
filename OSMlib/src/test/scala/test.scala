package org.seacourt.osm.test

import org.scalatest.{FunSuite}
import org.scalatest.matchers.ShouldMatchers
import scala.collection.{mutable, immutable}

class CoordTest extends FunSuite with ShouldMatchers
{
    import org.seacourt.osm._
    

    // lon, lat
    val appleton = Coord( -1.36184, 51.70928 )
	val wytham = Coord( -1.31186, 51.77466 )
    
    test("Haversine and equirectangular distance")
    {
    	val d1 = appleton.distFrom(wytham)
    	val d2 = appleton.approxDistFrom(wytham)
    	
    	d1 should be (d2 plusOrMinus 1e-2)
    }
    
    
    
    test("Trig restrictions")
    {
        import org.seacourt.osm.route._
        // Outside the rectangle
    	val yarnton = Coord( -1.30659, 51.80840 )
    	val tubney = Coord( -1.37020, 51.68558 )
    	
    	// In the rectangle
    	val cumnor = Coord( -1.33323, 51.73457 )
    	val tootBaldon = Coord( -1.18164, 51.69793 )
    	val radley = Coord( -1.23982, 51.68611 )
    	
    	{
	        val b = new BetwixValidator( appleton, wytham )
	        assert( b.check( appleton ) === true )
	        assert( b.check( wytham ) === true )
	        
	        assert( b.check( yarnton ) === false )
	        assert( b.check( tubney ) === false )
	        
	        assert( b.check( cumnor ) === true )
	        assert( b.check( tootBaldon ) === true )
    	}
    	
    	{
	        val b = new BetwixValidator( wytham, appleton )
	        assert( b.check( appleton ) === true )
	        assert( b.check( wytham ) === true )
	        
	        assert( b.check( yarnton ) === false )
	        assert( b.check( tubney ) === false )
	        
	        assert( b.check( cumnor ) === true )
	        assert( b.check( tootBaldon ) === true )
    	}
    	
    	{
	        val b = new BetwixValidator( tubney, cumnor )
	        assert( b.check( appleton ) === true )
	        assert( b.check( radley ) === true )
	        
	        assert( b.check( yarnton ) === false )
	        assert( b.check( wytham ) === false )
    	}
    	
    	{
	        val b = new BetwixValidator( cumnor, tubney )
	        assert( b.check( appleton ) === true )
	        assert( b.check( radley ) === true )
	        
	        assert( b.check( yarnton ) === false )
	        assert( b.check( wytham ) === false )
    	}
    }
}


class SRTMTest extends FunSuite
{
    import org.seacourt.osm._
    
    test("Interpolation etc")
    {
        val t = new ArcInfoAsciiInMemoryTile( 1.0, 2.0, 2, 2, 1.0, Array[Short]( 1, 2, 3, 4 ) )
        
        assert( t.interpolate( 1.0, 2.0 ) === Some(3.0) )
        assert( t.interpolate( 1.5, 2.0 ) === Some(2.0) )
        assert( t.interpolate( 1.0, 2.5 ) === Some(3.5) )
        assert( t.interpolate( 1.5, 2.5 ) === Some(2.5) )
    }
}

class BridgeFindingTest extends FunSuite
{
    import org.seacourt.osm.{Node, Coord}
    import org.seacourt.osm.route.{RouteNode, RouteEdge, EdgeDest, Bidirectional}
    
    class Graph
    {
        private var nextNodeId = 0
        private var nextEdgeId = 0
        
        def makeNode() : RouteNode =
        {
            val osmn = Node( Coord(0.0, 0.0), 0.0f, Array() )
            val nn = RouteNode( nextNodeId, osmn, 0.0f )
            nextNodeId += 1
            nn
        }
        
        def makeEdge( first : RouteNode, second : RouteNode ) : RouteEdge =
        {
            val e = RouteEdge( Map(), nextEdgeId, 0.0, 0.0, "", Array(), Array(), 0.0f, Array() )
            first.addEdge( second, e, Bidirectional, true )
            second.addEdge( first, e, Bidirectional, false )
            nextEdgeId += 1
            e
        }
    }
    
    // Based on: 'A Simple Test on 2-Vertex- and 2-Edge-Connectivity'
    private def findBridges( startNode : RouteNode ) =
    {
        var treeEdges = Set[Int]()
        var visitOrder = mutable.ArrayBuffer[RouteNode]()
        
        var nodeDepth = Map[Int, Int]()
        
        {
            var visitId = 1
            
            val q = mutable.Stack( (None : Option[RouteEdge], startNode) )

            // DFS to find a valid tree and to label all tree edges
            while ( !q.isEmpty )
            {
                val (headEdgeOption, head) = q.pop
                
                if ( !(nodeDepth contains head.nodeId) )
                {
                    nodeDepth += (head.nodeId -> visitId)
                    visitId += 1
                    headEdgeOption.foreach { e => treeEdges += e.edgeId }
                    visitOrder.append( head )

                    head.destinations.foreach
                    { d =>
                    
                        q.push( (Some(d.edge), d.node) )
                    }
                }
            }
        }
        
        // For each node, for each unlabelled edge, follow it away from the tree
        // using DFS to generate the chain decomposition.
        var visitedEdges = Set[Int]()
        var visitedNodes = Set[Int]()
        visitOrder.foreach
        { n =>
        
            n.destinations.foreach
            { d =>
            
                // Follow this edge back if it's not a tree edge
                val eid = d.edge.edgeId
                if ( !(treeEdges contains eid) && nodeDepth(d.node.nodeId) > nodeDepth(n.nodeId) )
                {
                    visitedNodes += n.nodeId
                    var destNodeO : Option[EdgeDest] = Some( d )
                    while ( destNodeO != None )
                    {
                        val de = destNodeO.get
                        val destNode = de.node
                        visitedEdges += de.edge.edgeId
                        
                        destNodeO = if ( !(visitedNodes contains destNode.nodeId) )
                        {
                            visitedNodes += destNode.nodeId

                            val nextDest = destNode.destinations.filter
                            { de =>
                                !(visitedEdges contains de.edge.edgeId) && nodeDepth(de.node.nodeId) < nodeDepth(destNode.nodeId)
                            }.headOption
                            
                            nextDest
                        }
                        else None
                    }
                }
            }
        }
        
        val bridgeEdgeIds = treeEdges.filter { eid => !(visitedEdges contains eid) }
        
        bridgeEdgeIds.toSet
    }
    
    test("Simple bridge test 1")
    {
        val g = new Graph()
        
        val N00 = g.makeNode()
        val N01 = g.makeNode()
        val N02 = g.makeNode()
        val N03 = g.makeNode()
        val N04 = g.makeNode()
        val N05 = g.makeNode()
        val N06 = g.makeNode()
        
        val E00 = g.makeEdge( N00, N01 )
        val E01 = g.makeEdge( N00, N03 )
        val E02 = g.makeEdge( N01, N02 )
        val E03 = g.makeEdge( N03, N02 )
        val E04 = g.makeEdge( N02, N04 )
        val E05 = g.makeEdge( N04, N05 )
        val E06 = g.makeEdge( N05, N06 )
        val E07 = g.makeEdge( N06, N04 )
        
        val bridges = findBridges( N00 )
        assert( bridges === Set(4) )
    }
    
    test("Simple bridge test 2")
    {
        val g = new Graph()
        
        val N00 = g.makeNode()
        val N01 = g.makeNode()
        val N02 = g.makeNode()
        val N03 = g.makeNode()
        val N04 = g.makeNode()
        val N05 = g.makeNode()
        val N06 = g.makeNode()
        val N07 = g.makeNode()
        val N08 = g.makeNode()
        val N09 = g.makeNode()
        val N10 = g.makeNode()
        val N11 = g.makeNode()
        val N12 = g.makeNode()
        val N13 = g.makeNode()
        
        
        val E00 = g.makeEdge( N00, N01 )
        val E01 = g.makeEdge( N00, N03 )
        val E02 = g.makeEdge( N01, N02 )
        val E03 = g.makeEdge( N03, N02 )
        val E04 = g.makeEdge( N02, N04 )
        val E05 = g.makeEdge( N04, N05 )
        val E06 = g.makeEdge( N05, N06 )
        val E07 = g.makeEdge( N06, N04 )
        
        val E08 = g.makeEdge( N05, N07 )
        val E09 = g.makeEdge( N07, N10 )
        val E10 = g.makeEdge( N07, N08 )
        val E11 = g.makeEdge( N07, N09 )
        val E12 = g.makeEdge( N09, N08 )
        val E13 = g.makeEdge( N10, N09 )
        
        val E14 = g.makeEdge( N00, N11 )
        val E15 = g.makeEdge( N13, N11 )
        val E16 = g.makeEdge( N12, N11 )
        val E17 = g.makeEdge( N12, N13 )
        
        
        val bridges = findBridges( N00 )
        assert( bridges === Set(4, 8, 14) )
    }
}
