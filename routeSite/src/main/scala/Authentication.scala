package org.seacourt.routeSite

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.Logging

// EH Cache support
import net.sf.ehcache.{CacheManager, Element}

// JSON handling support
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._


import org.json4s.native.Serialization.{read => sread, write => swrite}

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

