package org.seacourt.routeSite

import scala.collection.{mutable, immutable}

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Node, Coord, Logging}
import org.seacourt.osm.route.{RoutableGraph, RoutableGraphBuilder, RouteNode, RTreeIndex, ScenicPoint}

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener


// JSON handling support from Scalatra
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._


// In sbt:
//
// > container:start
// > ~ ;copy-resources;aux-compile

class RouteGraphHolder
{
    val rg = RoutableGraphBuilder.load( new java.io.File( "./default.bin.rg" ) )
}
// Weird cost:
// http://localhost:8080/displayroute?lon=-3.261151337280192&lat=54.45527013007099&distance=30.0&seed=1
class RouteSiteServlet extends ScalatraServlet with ScalateSupport with FlashMapSupport with Logging
{
    import net.sf.ehcache.{CacheManager, Element}
    
    implicit val formats = DefaultFormats
    
    Logging.configureDefaultLogging()

    private var rghOption : Option[RouteGraphHolder] = None
    
    private lazy val getRGH = new RouteGraphHolder()
    
    CacheManager.getInstance().addCache("memoized")
   
    
    def cached[T](name : String, args : Any* )( body : => T ) =
    {
        import java.security.MessageDigest
        

        def md5(s: String) : String =
        {
            MessageDigest.getInstance("SHA").digest(s.getBytes).map( x => "%02x".format(x) ).mkString
        }
        
        val hash = md5( name + "_" + List(args:_*).map( _.toString ).mkString("_") ).toString
        
        val cache = CacheManager.getInstance().getCache("memoized")   
        try
        {
            cache.acquireWriteLockOnKey( hash )
            
            val el = cache.get(hash)
            if ( el != null )
            {
                log.info( "Cached element found for: " + hash )
                
                el.getObjectValue.asInstanceOf[T]
            }
            else
            {
                log.info( "Cached element not found running function for: " + hash )
                val result = body
                
                cache.put( new Element( hash, result ) )
                
                result
            }
        }
        finally
        {
            cache.releaseWriteLockOnKey( hash )
        }
    }

    // So what do we do with this information?
    // Auth flow example: oauthssodemo.appspot.com/step/1
    
    // http://www.jaredarmstrong.name/2011/08/scalatra-an-example-authentication-app/ and 
    // http://www.jaredarmstrong.name/2011/08/scalatra-form-authentication-with-remember-me/
    get("/googleoauth2callback")
    {
        // Google etc incoming, reply of form:
        //http://www.two-pi.co.uk/googleoauth2callback#state=1234&access_token=ya29.AHES6ZQDbSARS6qaTmJJ_G7FQXBgSEwP8TJWc0OwDxvgl7V7ZRQrKQ&token_type=Bearer&expires_in=3600
        log.info( "Callback from google" )
    }

     
    def routeFilePath( routeId : String ) = new java.io.File("routes/route_%s.xml".format(routeId)).getAbsoluteFile
    
    def messageDigest( s : String ) = java.security.MessageDigest
        .getInstance("MD5")
        .digest(s.getBytes)
        .map("%02X".format(_))
        .mkString
    
    
    private def parseCoordPair( s : String ) =
    {
        val els = s.split(",").map(_.trim.toDouble)
        Coord( els(0), els(1) )
    }
    
    post("/requestroute")
    {
        import org.json4s.native.Serialization.{read => sread, write => swrite}
        implicit val formats = org.json4s.native.Serialization.formats(NoTypeHints)
        
        //contentType = "application/json"
        contentType = "text/plain"
        
        val startCoord = parseCoordPair( params("start") )
        val midCoordOption = params.get("mid").map( parseCoordPair )
        val distInKm = params("distance").toDouble
        
        println( "Request requestroute: %s, %s, %.2f".format( startCoord, midCoordOption, distInKm ) )
        
        val startNode = getRGH.rg.getClosest( startCoord )
        val midNodeOption = midCoordOption.map { mc => getRGH.rg.getClosest( mc ) }
        
        val res = getRGH.rg.buildRoute( startNode, midNodeOption, distInKm * 1000.0 ) match
        {
            case Some( route )  =>
            {
                log.info( "Route with %d directions".format( route.directions.size ) )
                route.directions.toList
            }
            case None           =>
            {
                log.error( "No route found" );
                List()
            }
        }
        
        val rendered = swrite(res)
        val hash = messageDigest( rendered )
        val pw = new java.io.PrintWriter( routeFilePath( hash ), "utf-8" )
        try
        {
            pw.print( rendered )
        }
        finally
        {
            pw.close
        }
        
        println( "Saving out " + hash )
        
        hash
    }
    
    get("/getroute/:routeId")
    {
        contentType = "application/json"
        
        println( "Request..." )
        
        val hash = params("routeId")
        
        println( "Hash: " + hash )
        
        routeFilePath( hash )
    }
    
    get("/")
    {
        redirect("/static/webapp.html")
    }
    
    
    
    /*get("/routegpx/:routeId")
    {
        contentType="text/xml"
        
        val routeId = params("routeId")
        val routeXML = scala.xml.XML.loadFile( routeFilePath( routeId ) )
        
        // Extract only the trk bit (the rest isn't valid gpx)
        routeXML \ "gpx"
    }*/
}

object JettyLauncher { // this is my entry object as specified in sbt project definition
  def main(args: Array[String]) {
    val port = if(System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080

    val server = new Server(port)
    val context = new WebAppContext()
    context setContextPath "/"
    //context.setResourceBase("src/main/webapp")
    val resourceBase = getClass.getClassLoader.getResource("webapp").toExternalForm
    context.setResourceBase(resourceBase)
    context.addEventListener(new ScalatraListener)
    context.addServlet(classOf[DefaultServlet], "/")

    server.setHandler(context)

    server.start
    server.join
  }
}

