package org.seacourt.routeSite

import scala.collection.{mutable, immutable}

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Node, Coord, Logging}
import org.seacourt.osm.route.{RoutableGraph, RoutableGraphBuilder, RouteNode, RouteEdge, RTreeIndex, ScenicPoint, POIType, Score}

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
    self : ScalatraServlet with FlashMapSupport with Logging =>
	
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
    
    def trackingCookie = cookies.get("JSESSIONID")
    
    protected def getUser : Option[User] =
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
    
    protected def logout()
    {
        trackingCookie.foreach { tc => getSessionCache.remove(tc) }
    }
    
    protected def login( userDetails : User )
    {
        trackingCookie.foreach { tc => getSessionCache.put( new Element(tc, userDetails, loginExpirySeconds, loginExpirySeconds) ) }
    }
    
}

trait GoogleAuthenticationSupport
{
	self : AuthenticationSupport with ScalatraServlet with FlashMapSupport with Logging =>
    
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
        val userDetails = existingUserOption match
        {
            case Some( user )   =>
            {
                flash("info") = "Welcome back: " + user.name
                user
            }
            case None           =>
            {
                val newUser = persistence.addUser( extId, userInfo.email, userInfo.name )
                flash("info") = "Thanks for joining: " + newUser.name
                newUser
            }
        }
        
        login( userDetails )
        
        redirect("/")
    }
    
    get("/guestLogin")
    {
        val extId = "guest"
            
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
                val newUser = persistence.addUser( extId, "guest@guest.com", "A guest" )
                flash("info") = "Thanks for joining: " + newUser.name
                newUser
            }
        }
        
        login( userDetails )
        
        redirect("/")
    }
    
    
}


class RouteSiteServlet( val persistence : Persistence ) extends ScalatraServlet
	with AuthenticationSupport
	with GoogleAuthenticationSupport
    with ScalateSupport
    with GZipSupport
    with FlashMapSupport
    with Logging
{
    implicit val formats = org.json4s.native.Serialization.formats(FullTypeHints( List(classOf[POIType]) ))
    
    private val loginExpirySeconds = 60*60*24*10

    def flashError( message : String )  { flash("error") = message }
    def flashInfo( message : String )   { flash("info") = message }
    
    //implicit val formats = DefaultFormats
    
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
            case Some(u) 	=> cookies += ("UserName" -> u.name)
            case None		=> cookies.delete("UserName")
        }
    }
    
    get("/logout")
    {
        getUser map
        { u =>
            trackingCookie.foreach
            { tc =>
                
                logout()
                
                flashInfo("Goodbye: " + u.name)
            }
        }
        
        redirect("/")
    }
    
    private val allCostModels = Map[String, RouteEdge => Score](
        "Walking"			-> (RouteEdge.walkingCost _),
        "Cycling"			-> (RouteEdge.cyclingCost( _, true ))
        //"Cycling (flat)"	-> (RouteEdge.cyclingCost( _, false )),
        //"Cycling (hilly)"	-> (RouteEdge.cyclingCost( _, true))
    )
    
    get("/costModels")
    {
        contentType = "application/json"
        
        swrite( allCostModels.map( _._1 ) )
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
        
        val costModel = allCostModels( costModelName )
        val startNode = rg.getClosest( costModel, startCoord )
        val midNodeOption = midCoordOption.map { mc => rg.getClosest( costModel, mc ) }
        
        
        rg.buildRoute( costModel, startNode, midNodeOption, distInKm * 1000.0 ) match
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
    
    get("/debugroute")
    {
        contentType = "application/json"
            
        val startCoord = parseCoordPair( params("start") )
        val distInKm = params("distance").toDouble
        val costModelName = params("model")
        
        log.info( "Request requestroute: %s, %.2f, %s".format( startCoord, distInKm, costModelName ) )
        
        val costModel = allCostModels( costModelName )
        val startNode = rg.getClosest( costModel, startCoord )
        val res = rg.debugRoute( costModel, startNode, distInKm * 1000.0 )
        
        val serialised = swrite(res)
        log.info( "Debug size: %.2fMb".format( serialised.size.toDouble / (1024.0 * 1024.0) ) )
        serialised
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
                
                val res = swrite(routes)
                println( routes )
                println( res )
                
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
            "flash" -> flash,
            "user" -> user
        )
    }
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

