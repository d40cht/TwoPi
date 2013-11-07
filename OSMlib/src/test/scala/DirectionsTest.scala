package org.seacourt.osm.test

import org.scalatest.{FlatSpec}
import org.scalatest.matchers.ShouldMatchers
import scala.collection.{mutable, immutable}

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import org.seacourt.osm.{Coord, Node}
import org.seacourt.osm.route.{FoundRoute, RouteResult, RouteDirections, POI, POIType, EdgeAndBearing}


case class MinimalRouteSection( val parentEdgeBearing : EdgeAndBearing, val direction : Option[String] )
{
    var nodes = mutable.ArrayBuffer[Node]()
}

class DirectionsTest extends FlatSpec with ShouldMatchers
{
	import org.json4s.native.Serialization.{read => sread, write => swrite}
    implicit val formats = org.json4s.native.Serialization.formats(FullTypeHints( List(classOf[POIType]) ))
    
    "Directions generator" should "produce decent walking directions with barriers, gates, lane crossings etc annotated" in
    {
        val data = scala.io.Source.fromFile( new java.io.File("OSMlib/src/test/data/WythamMillWalk.json") )
        val wythamRoute = sread[FoundRoute]( data.reader() )
        
        //println( wythamRoute.path.size )

        val minimalSections =
        {
	        val sectionBuffer = mutable.ArrayBuffer[MinimalRouteSection]()
	        wythamRoute.path.foreach
	        { fpe =>
	            
	        	val ra = fpe.routeAnnotation
	        	val outgoingEBOption = fpe.outgoingEB
	        	
	        	println( "Zig: " + ra.toString )
	        	println( "Zog: " + fpe.outgoingEB.toString )
	        	outgoingEBOption match
	        	{
	        	    case Some(outgoingEB) =>
	    	        {
	    	        	println( "Foobar" )
	    	            var latestSection = MinimalRouteSection( outgoingEB, None )
	    	        	sectionBuffer.append( latestSection )
	    	        	
	    	            val ed = outgoingEB.edgeDest
	    	            
	    	            val routeEdge = ed.edge
	    	            
	    	            val edgeName = routeEdge.name
	    	            
	    	            routeEdge.nodes.foreach
	    	            { n =>
	    	                
	    	                val barrierTagOption = n.tagMap.get("barrier")
	    	                
	    	                barrierTagOption match
	    	                {
	    	                    case Some( barrierName ) =>
		                        {
		                            val barrierSection = MinimalRouteSection( outgoingEB, Some( "Cross a " + barrierName ) )
		                            barrierSection.nodes.append( n )
		                            sectionBuffer.append( barrierSection )
		                            latestSection = MinimalRouteSection( outgoingEB, None )
		                        }
	    	                    case None =>
		                        {
		                        	latestSection.nodes.append( n )
		                        }
	    	                }
	    	                // stile, gate, cattle_grid etc
	    	                
	    	            }
	    	        }
	    	        
	        	    case None =>
	        	}
	        }
	        
	        sectionBuffer//.filter( !_.nodes.isEmpty )
        }
        
        minimalSections.foreach
        { ms =>
            
            val heightDelta = ms.nodes.last.height - ms.nodes.head.height 
            val distance = ms.nodes.sliding(2).map( p => p(1).coord.distFrom( p(0).coord ) ).sum
            val inclinePercent = 100.0 * (heightDelta/distance)
            
            println( "%s, %s, %.2f%%".format( ms.parentEdgeBearing.edgeDest.edge.name, ms.direction.toString, inclinePercent ) )
        }
    }
}