package org.seacourt.routeSite

import scala.collection.{mutable, immutable}

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Node, Coord, Logging}
import org.seacourt.osm.route.{RoutableGraph, RoutableGraphBuilder, RouteNode, RTreeIndex, ScenicPoint, POIType}

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener


// JSON handling support from Scalatra
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

// Slick support
import scala.slick.session.Database
import Database.threadLocalSession

import org.seacourt.osm.route.{RouteResult}

// In sbt:
//
// > container:start
// > ~ ;copy-resources;aux-compile


class GeographImageServlet extends ScalatraServlet
{
    private val geoGraphCache = new java.io.File( "geographCache" )
    if ( !geoGraphCache.exists() ) geoGraphCache.mkdirs()
    
    private def imgUrl( index : Long, hash : String ) : String =
    {
        val yz = index / 1000000
        val ab = (index % 1000000) / 10000
        val cd = (index % 10000) / 100
        
        val fullPath = if ( yz == 0 )
        {
            "/photos/%02d/%02d/%06d_%s".format( ab, cd, index, hash )
        }
        else
        {
            "/geophotos/%02d/%02d/%02d/%06d_%s".format( yz, ab, cd, index, hash )
        }
        
        fullPath
    }
    
    private def cacheToFile( url : String, fileName : java.io.File ) =
    {
        import scalaj.http.{Http, HttpOptions}
        val res = Http(url)
            .option(HttpOptions.connTimeout(500))
            .option(HttpOptions.readTimeout(500))
        { inputStream =>
        
            val os = new java.io.FileOutputStream( fileName )
            try
            {   
                org.apache.commons.io.IOUtils.copy( inputStream, os )
            }
            finally
            {
                os.close
            }
        }
    }
    
    get("/thumb/:id/:hash")
    {
        contentType = "image/jpeg"
        
        val id = params("id").toLong
        val hash = params("hash")
        val fullPath = imgUrl( id, hash )
        
        val url = "http://s%d.geograph.org.uk%s_213x160.jpg".format( id % 4, fullPath )
        val fname = new java.io.File( geoGraphCache, "%d_thumb.jpg".format( id ) )
        cacheToFile( url, fname )
        
        fname
    }
    
    get("/full/:id/:hash")
    {
        contentType = "image/jpeg"
        
        val id = params("id").toLong
        val hash = params("hash")
        val fullPath = imgUrl( id, hash )
        
        val url = "http://s0.geograph.org.uk%s.jpg".format( fullPath )
        val fname = new java.io.File( geoGraphCache, "%d.jpg".format( id ) )
        cacheToFile( url, fname )
        
        fname
    }
}

class RouteGraphHolder
{
    val rg = RoutableGraphBuilder.load( new java.io.File( "./default.bin.rg" ) )
}

// Weird cost:
// http://localhost:8080/displayroute?lon=-3.261151337280192&lat=54.45527013007099&distance=30.0&seed=1
class RouteSiteServlet( val persistence : Persistence ) extends ScalatraServlet
    with ScalateSupport
    //with GZipSupport
    with FlashMapSupport
    with Logging
{
    import net.sf.ehcache.{CacheManager, Element}
    import org.json4s.native.Serialization.{read => sread, write => swrite}
    implicit val formats = org.json4s.native.Serialization.formats(FullTypeHints( List(classOf[POIType]) ))
    
    private val loginExpirySeconds = 60*60*24*10

    def flashError( message : String )  { flash("error") = message }
    def flashInfo( message : String )   { flash("info") = message }
    
    //implicit val formats = DefaultFormats
    
    Logging.configureDefaultLogging()

    private var rghOption : Option[RouteGraphHolder] = None
    
    private lazy val getRGH = new RouteGraphHolder()
     
    private def getSessionCache =
    {
        var g = CacheManager.getInstance().getCache("sessions")
        
        if ( g == null )
        {
            CacheManager.getInstance().addCache("sessions")
            CacheManager.getInstance().getCache("sessions")
        }
        else
        {
            g
        }    
    }     

    
    private val trackingCookieName = "routeSite"
    private val oneWeek = 7*24*60*60
    
    before()
    {
        // If there is no tracking cookie, add one so we can handle
        // persistent sessions, oath etc
    	if ( !cookies.get(trackingCookieName).isDefined )
    	{
    		val currTrackingId = Some(java.util.UUID.randomUUID.toString)
    		//log.info( "Setting tracking cookie to: " + currTrackingId )
    	    response.addHeader("Set-Cookie",
    	    	Cookie(trackingCookieName, currTrackingId.get)(CookieOptions(secure=false, maxAge=oneWeek)).toCookieString)
    	}
    	/*else
    	{
    	    log.info( "Found tracking cookie id: " + cookies.get(trackingCookieName).get)
    	}*/
    }
    
    def trackingCookie = cookies.get(trackingCookieName)
    
    private val googleRedirectURI="http://www.twopi.co.uk/googleoauth2callback"
	private val googleClientId = "725550604793.apps.googleusercontent.com"
	private val googleClientSecret = "mYcfxnq8nrSDe8iCP9qN9TWn"
    
    // http://www.jaredarmstrong.name/2011/08/scalatra-an-example-authentication-app/ and 
    // http://www.jaredarmstrong.name/2011/08/scalatra-form-authentication-with-remember-me/
    get("/googleoauth2callback")
    {
        // From: https://developers.google.com/accounts/docs/OAuth2WebServer
        import scalaj.http.{Http, HttpOptions}
        import net.liftweb.json.{JsonParser, DefaultFormats, JObject}
        implicit val formats = DefaultFormats
        
        params.get("error") foreach
        { message =>

            flash("error") = message
            redirect("/")
        }
        
        // Now get auth code
        val code = params("code")
        
        log.info( "Requesting auth code" )
        val authCodeRes = Http.post("https://accounts.google.com/o/oauth2/token")
        	.params(
    	        "code" 			-> code,
    	        "client_id" 	-> googleClientId,
    	        "client_secret"	-> googleClientSecret,
    	        "redirect_uri"	-> googleRedirectURI,
    	        "grant_type"	-> "authorization_code" )
    	    { inputStream =>
        
                val writer = new java.io.StringWriter()
                org.apache.commons.io.IOUtils.copy( inputStream, writer )
                val res = writer.toString()
            	JsonParser.parse( res )
            
    	    }
        
        // Post once again to get user information
        log.info( "Requesting user information" )
        val accessToken = (authCodeRes \\ "access_token").extract[String]
        val url = "https://www.googleapis.com/oauth2/v1/userinfo?access_token=" + accessToken
        val resJSON = Http(url)
            .option(HttpOptions.connTimeout(500))
            .option(HttpOptions.readTimeout(500))
        { inputStream =>
        
            val writer = new java.io.StringWriter()
            org.apache.commons.io.IOUtils.copy( inputStream, writer )
            val res = writer.toString()
        	JsonParser.parse( res )
        }
        
        val id 			= (resJSON \\ "id").extract[String]
        val email 		= (resJSON \\ "email").extract[String]
        val name 		= (resJSON \\ "name").extract[String]
        /*val givenName 	= (resJSON \\ "given_name").extract[String]
        val familyName	= (resJSON \\ "family_name").extract[String]
        val profileLink	= (resJSON \\ "link").extract[String]
        val gender		= (resJSON \\ "gender").extract[String]
        
        log.info( "Google user info: " + (id, email, name, givenName, familyName, profileLink, gender).toString )*/
        log.info( "Google user info: " + (id, email, name).toString )
        
        val extId = "google_" + id

        val existingUserOption = persistence.getUser(extId)
        val userDetails = existingUserOption match
        {
            case Some( user )   =>
            {
                flash("info") = "Welcome back: " + user.name
                user
            }
            case None           =>
            {
                val newUser = persistence.addUser( extId, email, name )
                flash("info") = "Thanks for joining: " + newUser.name
                newUser
            }
        }
        
        trackingCookie.foreach
        { tc =>
            
            getSessionCache.put( new Element(tc, userDetails, loginExpirySeconds, loginExpirySeconds) )
        }
        
        redirect("/")
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

    private def getUser : Option[User] =
    {
        trackingCookie.flatMap
        { tc =>
            
            val userData = getSessionCache.get(tc)
            if ( userData == null ) None
            else
            {
            	Some( userData.getObjectValue.asInstanceOf[User] )
            }
        }
    }
    
    get("/logout")
    {
        getUser map
        { u =>
            trackingCookie.foreach
            { tc =>
                
                getSessionCache.remove(tc)
                
                flashInfo("Goodbye " + u.name)
            }
        }
        
        redirect("/")
    }
    
    post("/requestroute")
    {
        //contentType = "application/json"
        contentType = "text/plain"
        
        val startCoord = parseCoordPair( params("start") )
        val midCoordOption = params.get("mid").map( parseCoordPair )
        val distInKm = params("distance").toDouble
        
        log.info( "Request requestroute: %s, %s, %.2f".format( startCoord, midCoordOption, distInKm ) )
        
        val startNode = getRGH.rg.getClosest( startCoord )
        val midNodeOption = midCoordOption.map { mc => getRGH.rg.getClosest( mc ) }
        
        getRGH.rg.buildRoute( startNode, midNodeOption, distInKm * 1000.0 ) match
        {
            case Some( route : RouteResult )  =>
            {
                log.info( "Route with %d directions".format( route.directions.size ) )
                val jsonRendered = swrite(route)
                
                val routeId = persistence.addRoute(jsonRendered, route.distance, route.ascent)
                
                routeId.toString
            }
            case None           =>
            {
                log.error( "No route found" );
                "Error"
            }
        }
        
        
    }
    
    get("/getroute/:routeId")
    {
        contentType = "application/json"
        
        
        persistence.getRoute( params("routeId").toInt ) match
        {
            case Some(routeData)    => routeData
            case None               => "Error: route not found"
        }
    }
    
    get("/gpx/:routeId")
    {   
        persistence.getRoute( params("routeId").toInt ) match
        {
            case Some(routeData)    =>
            {
                val routeResult = sread[RouteResult]( routeData )
                <gpx>
                    <name>Example route</name>
                    <trk><trkseg>
                    {
                        routeResult.directions.flatMap
                        { rd =>
                            
                            rd.inboundNodes.map
                            { n =>
                            
                                <trkpt lat={n.node.coord.lat.toString} lon={n.node.coord.lon.toString}>
                                    <ele>{n.node.height.toString}</ele>
                                </trkpt>
                            }
                        }
                    }
                    </trkseg></trk>
                </gpx>
            }
            case None               =>
            {
                <gpx></gpx>
            }
        }
    }
    
    get("/saveroute/:routeId/:routeName")
    {
        getUser match
        {
            case Some(user) =>
            {
                val routeId = params("routeId").toInt
                val routeName = params("routeName")
                persistence.saveRouteToUser( user.id, routeId, routeName )
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
                
                swrite(routes)
            }
            case None       => "fail"
        }
    }

    get("/")
    {
        redirect("./app")
    }
    
    get("/app*")
    {
        import org.scalatra.util.RicherString._
        
        templateEngine.allowReload = false
        templateEngine.allowCaching = true
        
        contentType="text/html"
            
		val googleOpenIdLink="https://accounts.google.com/o/oauth2/auth?response_type=code&scope=%s&client_id=%s&redirect_uri=%s&access_type=online".format(
		    "https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile",
		        googleClientId,
		        googleRedirectURI )
		        
        val user = getUser
        
        layoutTemplate("/WEB-INF/templates/views/appframe.ssp",
            "googleOpenIdLink" -> googleOpenIdLink.urlEncode,
            "flash" -> flash,
            "user" -> user
        )
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

