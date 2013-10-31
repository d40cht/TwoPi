package org.seacourt.routeSite

import scala.collection.{mutable, immutable}

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Node, Coord, Logging, RTreeIndex}
import org.seacourt.osm.route.{RoutableGraph, RoutableGraphBuilder, RouteNode, RouteEdge, ScenicPoint, POIType, Score, RouteType}

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener

/*
http://www.geograph.org.uk/more.php?id=3331644
http://www.geograph.org.uk/more.php?id=2792162
http://www.geograph.org.uk/photo/1041947

http://www.geograph.org.uk/more.php?id=3177083
http://www.geograph.org.uk/more.php?id=2113291


Big:

Trevor Littlewood: http://www.geograph.org.uk/profile/39198

http://www.geograph.org.uk/reuse.php?id=2151547&download=b276cd82&size=original
http://www.geograph.org.uk/more.php?id=3686911
http://www.geograph.org.uk/profile/39198
http://www.geograph.org.uk/photo/3686492
http://www.geograph.org.uk/photo/3679887
http://www.geograph.org.uk/photo/3564088
http://www.geograph.org.uk/photo/3522426

Appropriate:

http://www.geograph.org.uk/more.php?id=3703973

http://www.geograph.org.uk/more.php?id=3196713

*/

// JSON handling support
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

// Slick support
import scala.slick.session.Database
import Database.threadLocalSession

// EH Cache support
import net.sf.ehcache.{CacheManager, Element}


import org.seacourt.osm.route.{RouteResult}

import org.json4s.native.Serialization.{read => sread, write => swrite}



// In sbt:
//
// > container:start
// > ~ ;copy-resources;aux-compile


case class JSONResponse[T]( val status : String, val message : Option[String], val data : T )

class RouteSiteServlet( val persistence : Persistence ) extends ScalatraServlet
	with AuthenticationSupport
	with GoogleAuthenticationSupport
    with ScalateSupport
    with GZipSupport
    with Logging
{
    implicit val formats = org.json4s.native.Serialization.formats(FullTypeHints( List(classOf[POIType]) ))
    
    private val loginExpirySeconds = 60*60*24*10
    
    val rg = RoutableGraphBuilder.load( new java.io.File( "./default.bin.rg" ) )
     
    
    private def parseCoordPair( s : String ) =
    {
        val els = s.split(",").map(_.trim.toDouble)
        Coord( els(0), els(1) )
    }
    
    error
    {
        case e =>
        {
            val sw = new java.io.StringWriter()
            val pw = new java.io.PrintWriter(sw)
            e.printStackTrace(pw)
            log.warning("an exception occurred: " + sw.toString)
            log.warning("the request body is: " + request)
            NotFound("An error occurred, please contact support")
        }
    }

    before()
    {
        log.debug( "Request from %s for %s (referrer %s)".format( request.remoteAddress, request.pathInfo, request.referrer ) )
        
        getUser match
        {
            case Some(u) 	=>
            {
                cookies += ("UserName" -> u.name)
                cookies += ("UserId" -> u.id.toString)
            }
            case None		=>
            {
                cookies.delete("UserName")
                cookies.delete("UserId")
            }
        }
    }
    
    
    get("/costModels")
    {
        contentType = "application/json"
        
        swrite( RouteType.allTypes.map( _._1 ) )
    }
    
    post("/requestroute")
    {
        //contentType = "application/json"
        contentType = "text/plain"
        
        val startCoord = parseCoordPair( params("start") )
        val midCoordOption = params.get("mid").map( parseCoordPair )
        val distInKm = params("distance").toDouble
        val costModelName = params("model")
        
        log.info( "Request requestroute: %s, %s, %.2f, %s".format( startCoord, midCoordOption, distInKm, costModelName ) )
        
        val routeType = RouteType.allTypes( costModelName )
        val startNode = rg.getClosest( routeType, startCoord )
        val midNodeOption = midCoordOption.map { mc => rg.getClosest( routeType, mc ) }
        
        
        rg.buildRoute( routeType, startNode, midNodeOption, distInKm * 1000.0 ) match
        {
            case Some( route : RouteResult )  =>
            {
                log.info( "Route with %d directions".format( route.directions.size ) )
                val jsonRendered = swrite(route)
                
                val userIdOption = getUser.map { u => u.id }
        

                val routeId = persistence.addRoute(jsonRendered, startCoord, route.routeType, route.distance, route.ascent, route.duration, userIdOption)
                
                routeId.toString
            }
            case None           =>
            {
                log.error( "No route found" )
                "Error"
            }
        }  
    }
    
    get("/debugroute")
    {
        contentType = "application/json"
            
        val startCoord = parseCoordPair( params("start") )
        val distInKm = params("distance").toDouble
        val costModelName = params("model")
        
        log.info( "Request requestroute: %s, %.2f, %s".format( startCoord, distInKm, costModelName ) )
        
        val routeType = RouteType.allTypes( costModelName )
        val startNode = rg.getClosest( routeType, startCoord )
        val res = rg.debugRoute( routeType, startNode, distInKm * 1000.0 )
        
        val serialised = swrite(res)
        log.info( "Debug size: %.2fMb".format( serialised.size.toDouble / (1024.0 * 1024.0) ) )
        serialised
    }
    
    get("/getroute/:routeId")
    {
        contentType = "application/json"
        
        val routeData = persistence.getRoute( params("routeId").toInt )
        
        
        persistence.getRoute( params("routeId").toInt ) match
        {
            case Some(routeData)    =>
            {
                val routeName = persistence.getRouteName( params("routeId").toInt )
                val nr = NamedRoute( routeName, routeData )
                swrite(nr)
            }
            case None               => "Error: route not found"
        }
    }
    
    get("/getroutesummary/:routeId")
    {
        contentType = "application/json"
        
        persistence.getRouteSummary( params("routeId").toInt ) match
        {
            case Some(routeData)    => swrite(routeData)
            case None               => "Error: route not found"
        }
    }
    
    get("/gpx/:routeId")
    {
        contentType = "text/xml"

        val routeName = "Example route"
        persistence.getRoute( params("routeId").toInt ) match
        {
            case Some(routeResult)    =>
            {
                <gpx>
                    <name>{routeName}</name>
                    <cmt>{routeName}</cmt>
                    <desc>{routeName}</desc>
                    <trk><trkseg>
                    {
                        routeResult.directions.flatMap
                        { rd =>
                            
                            rd.outboundNodes.map
                            { n =>
                            
                                <trkpt lat={n.node.coord.lat.toString} lon={n.node.coord.lon.toString}>
                                    <ele>{n.node.height.toString}</ele>
                                </trkpt>
                            }
                        }
                    }
                    </trkseg></trk>
                    
                    <rte>
                        <name>{routeName}</name>
                        <cmt>{routeName}</cmt>
                        <desc>{routeName}</desc>
                        {
                            routeResult.directions.map
                            { rd =>
                            
                                <rtept>
                                    <name>{rd.directionsText}</name>
                                    <cmt>{rd.directionsText}</cmt>
                                    <desc>{rd.directionsText}</desc>
                                    <lat>{rd.coord.lat.toString}</lat>
                                    <lon>{rd.coord.lon.toString}</lon>
                                </rtept>
                            }
                        }
                    </rte>
                </gpx>
            }
            case None               =>
            {
                <gpx></gpx>
            }
        }
    }
    
    post("/saveroute")
    {
        getUser match
        {
            case Some(user) =>
            {
                val routeId = params("id").toInt
                val routeName = params("name")
                val description = params("description")
                persistence.nameRoute( user.id, routeId, routeName, description )
                "success"
            }
            case None =>
            {
                "fail"
            }
        }
    }
    
    get("/myroutes")
    {
        contentType = "application/json"
        getUser match
        {
            case Some(user) =>
            {
                val routes = persistence.getUserRoutes( user.id )
                
                val res = swrite(routes)
                
                res
            }
            case None       => "fail"
        }
    }
    
    get("/allroutes")
    {
        contentType = "application/json"
        
        val routes = persistence.getAllNamedRoutes()
        swrite(routes)
    }

    get("/")
    {
        redirect("./app/splash")
    }
    
    get("/app*")
    {
        import org.scalatra.util.RicherString._
        
        templateEngine.allowReload = false
        templateEngine.allowCaching = true
        
        contentType="text/html"
		        
        val user = getUser

        log.info( "User: " + user )
        
        layoutTemplate("/WEB-INF/templates/views/appframe.ssp",
            "googleOpenIdLink" -> googleOpenIdLink.urlEncode,
            "user" -> user
        )
    }
    
    /*notFound
    {
          <h1>Not found. Bummer.</h1>
    }*/
}

object JettyLauncher
{
	def main(args: Array[String])
	{
		val port = if(System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080
		val server = new Server(port)
		val context = new WebAppContext()
		context.setInitParameter("org.mortbay.jetty.servlet.SessionURL", "none")
		context setContextPath "/"
		

		val resourceBase = getClass.getClassLoader.getResource("webapp").toExternalForm
		println( "Resource base: " + resourceBase)
		context.setResourceBase(resourceBase)
		context.setTempDirectory( new java.io.File("./contexttmp") )
		context.setExtractWAR( true )
		context.addEventListener(new ScalatraListener)
		context.addServlet(classOf[DefaultServlet], "/")

	    server.setHandler(context)
	
	    server.start
	    server.join
	}
}

