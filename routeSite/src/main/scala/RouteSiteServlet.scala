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
            .option(HttpOptions.connTimeout(1500))
            .option(HttpOptions.readTimeout(1500))
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

 
case class GoogleUserInfo(
    val id 			: String,
    val email 		: String,
    val name 		: String,
    val givenName 	: Option[String],
    val familyName 	: Option[String],
    val profileLink	: Option[String],
    val gender		: Option[String] )


trait AuthenticationSupport
{
    self : ScalatraServlet with Logging =>
	
    def persistence : Persistence
        
    private final val loginExpirySeconds = 10 * 24 * 60 * 60
    private final val sessionCacheName = "sessions"
	    
    private def getSessionCache =
    {
        CacheManager.getInstance().getCache(sessionCacheName) match
        {
            case null 			=>
            {
            	CacheManager.getInstance().addCache(sessionCacheName)
                CacheManager.getInstance().getCache(sessionCacheName)
            }
            case existingCache	=> existingCache
        }    
    }     
    
    def trackingCookie : String =
    {
        val trackingId = "TwoPISession"
        cookies.get(trackingId) match
        {
            case Some(tc)       => tc
            case None           =>
            {
                val uuid = java.util.UUID.randomUUID().toString
                cookies += (trackingId -> uuid)
                uuid
            }
        }
    }
    
    protected def getUser : Option[User] =
    {
        val tc = trackingCookie
    
        val userData = getSessionCache.get(tc)
        if ( userData == null ) None
        else
        {
        	Some( userData.getObjectValue.asInstanceOf[User] )
        }
    }
    
    protected def logout()
    {
        getSessionCache.remove(trackingCookie)
    }
    
    protected def login( userDetails : User )
    {
        getSessionCache.put( new Element(trackingCookie, userDetails, loginExpirySeconds, loginExpirySeconds) )
    }
    
}

trait GoogleAuthenticationSupport
{
	self : AuthenticationSupport with ScalatraServlet with Logging =>
    
    private final val googleRedirectURI="http://www.twopi.co.uk/googleoauth2callback"
	private final val googleClientId = "725550604793.apps.googleusercontent.com"
	private final val googleClientSecret = "mYcfxnq8nrSDe8iCP9qN9TWn"

        
    def googleOpenIdLink="https://accounts.google.com/o/oauth2/auth?response_type=code&scope=%s&client_id=%s&redirect_uri=%s&access_type=online".format(
	    "https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile",
	        googleClientId,
	        googleRedirectURI )
	        
    get("/googleoauth2callback")
    {
        // From: https://developers.google.com/accounts/docs/OAuth2WebServer
        import scalaj.http.{Http, HttpOptions}
        import net.liftweb.json.{JsonParser, DefaultFormats, JObject}
        implicit val formats = DefaultFormats
        
        params.get("error") foreach
        { message =>

            redirect("/app/error/" + message)
        }
        
        // Now get auth code
        val code = params("code")
        
        // And return state
        val redirectBackTo = params("state")
        
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
        val userInfo = Http(url)
            .option(HttpOptions.connTimeout(500))
            .option(HttpOptions.readTimeout(500))
        { inputStream =>
        
            implicit val formats = org.json4s.native.Serialization.formats(NoTypeHints)
            
            val writer = new java.io.StringWriter()
            org.apache.commons.io.IOUtils.copy( inputStream, writer )
            val res = writer.toString()
            
        	sread[GoogleUserInfo]( res )
        }
       
            
        log.info( "Google user info: " + userInfo.toString )
        
        val extId = "google_" + userInfo.id

        val existingUserOption = persistence.getUser(extId)
        existingUserOption match
        {
            case Some( user )   =>
            {
                login( user )
                redirect("/app/flash?message=Welcome back " + user.name + "&redirect=" + redirectBackTo)
            }
            case None           =>
            {
                val newUser = persistence.addUser( extId, userInfo.email, userInfo.name )
                login( newUser )
                redirect("/app/flash/?message=Welcome " + newUser.name + "&redirect=" + redirectBackTo)
            }
        }
    }
    
    get("/guestLogin")
    {
        val extId = "guest"
        
        val redirectBackTo = params("state")
            
        val existingUserOption = persistence.getUser(extId)
        existingUserOption match
        {
            case Some( user )   =>
            {
                login(user)
                redirect("/app/flash?message=Welcome back " + user.name + "&redirect=" + redirectBackTo)
            }
            case None           =>
            {
                val newUser = persistence.addUser( extId, "guest@guest.com", "A guest" )
                login(newUser)
                redirect("/app/flash/?message=Welcome " + newUser.name + "&redirect=" + redirectBackTo)
            }
        }
        
        redirect(redirectBackTo)
    }
    
    get("/logout")
    {
        val redirectBackTo = params("state")
        
        getUser map
        { u =>
            trackingCookie.foreach
            { tc =>
                
                logout()
            }
        }
        
        redirect(redirectBackTo)
    }
    
    
}


class RouteSiteServlet( val persistence : Persistence ) extends ScalatraServlet
	with AuthenticationSupport
	with GoogleAuthenticationSupport
    with ScalateSupport
    with GZipSupport
    with Logging
{
    implicit val formats = org.json4s.native.Serialization.formats(FullTypeHints( List(classOf[POIType]) ))
    
    private val loginExpirySeconds = 60*60*24*10
    
    Logging.configureDefaultLogging()

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

