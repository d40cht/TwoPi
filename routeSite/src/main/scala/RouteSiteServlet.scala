package org.seacourt.routeSite

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Coord}
import org.seacourt.osm.route.RoutableGraph

// In sbt:
//
// > container:start
// > ~ ;copy-resources;aux-compile

class RouteSiteServlet extends ScalatraServlet with ScalateSupport
{
    val mapFile = new java.io.File( "/home/alex.wilson/Devel/AW/OSM-Crunch/oxfordshire.bin" )
        
    val map = OSMMap.load( mapFile )
    val rg = RoutableGraph( map )

    get("/")
    {
        <html>
            <body>
                <h1>Hello, world!</h1>
                Say <a href="hello-scalate">hello to Scalate</a>.
            </body>
        </html>
    }
  
    get("/hello-scalate")
    {
        <html>
            <body> 
                <h1>Why, thank-you</h1>
            </body>
        </html>
    }
    
    // e.g. localhost:8080/route?data=-1.3611464,51.7094267,30.0,1
    get("/route")
    {
        val els = params("data").split(",")
        
        val lon = els(0).toDouble
        val lat = els(1).toDouble
        val distInKm = els(2).toDouble
        val seed = els(3).toInt
        
        val startCoords = Coord(lon, lat)
        
        println( "Finding closest node..." )
        val closestNode = rg.getClosest( startCoords )
        
        println( "Closest: " + closestNode.node.coord )
        
        val routeNodes = rg.buildRoute( closestNode, distInKm * 1000.0, seed )
        
        routeNodes.map
        { rn =>
            "%f,%f".format( rn.node.coord.lat, rn.node.coord.lon )
        }.mkString("\n")
    }
}

