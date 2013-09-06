package org.seacourt.routeSite

import scala.collection.{mutable, immutable}

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Coord, Logging}
import org.seacourt.osm.route.{RoutableGraph, RouteNode, RTreeIndex, ScenicPoint}

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener

// In sbt:
//
// > container:start
// > ~ ;copy-resources;aux-compile




class RouteGraphHolder
{
    val rg = RoutableGraph.load( new java.io.File( "./default.bin.rg" ) )
}
// Weird cost:
// http://localhost:8080/displayroute?lon=-3.261151337280192&lat=54.45527013007099&distance=30.0&seed=1
class RouteSiteServlet extends ScalatraServlet with ScalateSupport with Logging
{
    import net.sf.ehcache.{CacheManager, Element}
    
    Logging.configureDefaultLogging()

    private var rghOption : Option[RouteGraphHolder] = None
    
    private def getRGH =
    {
        if ( rghOption.isEmpty ) rghOption = Some( new RouteGraphHolder() )
        rghOption.get
    }
    
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
    
    val letters = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    
    private def getRouteXML( lon : Double, lat : Double, distInKm : Double, seed : Int ) = cached[scala.xml.Node]("routeXML", lon, lat, distInKm, seed )
    {
        import net.liftweb.json.{JsonParser, DefaultFormats, JObject}
        import scalaj.http.{Http, HttpOptions}
        implicit val formats = DefaultFormats
        
        val rgh = getRGH
        val startCoords = Coord(lon, lat)
        
        log.info( "Finding closest node..." )
        val closestNode = rgh.rg.getClosest( startCoords )
        
        log.info( "Closest: " + closestNode.coord )
        
        val route = rgh.rg.buildRoute( closestNode, distInKm * 1000.0, seed )
        val routeNodes = route.routeNodes
        val pics = route.picList
        
        var lastRN : Option[RouteNode] = None
        var cumDistance = 0.0
        <gpx>
            <trk>
                <name>Example route</name>
                <trkseg>
                {
                    routeNodes.map
                    { rn =>
                    
                        val dist = lastRN match
                        {
                            case Some( lrn ) => lrn.coord.distFrom( rn.coord )
                            case None => 0.0
                        }
                        cumDistance += dist
                        
                        val res = <trkpt lat={rn.coord.lat.toString} lon={rn.coord.lon.toString} distance={cumDistance.toString} ele={rn.height.toString}/>
                        lastRN = Some( rn )
                        res
                    }
                }
                </trkseg>
            </trk>
            <pics>
            {   
                pics.zip(letters).map
                { case (pic, letter) =>

                    val picIndex = pic.picIndex
                    
                    try
                    {
                        val resJSON = Http("http://jam.geograph.org.uk/sample8.php?q=&select=title,grid_reference,realname,user_id,hash&range=%d,%d".format( picIndex, picIndex ))
                            .option(HttpOptions.connTimeout(500))
                            .option(HttpOptions.readTimeout(500))
                        { inputStream => 
                            JsonParser.parse(new java.io.InputStreamReader(inputStream))
                        }
                        
                        val imgMatches = (resJSON \\ "matches")
                        val imgMetaData = imgMatches.asInstanceOf[JObject].obj.head.value
        
                        val title = (imgMetaData \\ "title").extract[String]
                        val authorName = (imgMetaData \\ "realname").extract[String]
                        val hash = (imgMetaData \\ "hash").extract[String]
                        
                        val imageUrl = imgUrl( picIndex, hash )
                        
                        val link = "http://www.geograph.org.uk/photo/" + pic.picIndex
                        val letterLink = "/img/mapMarkers/paleblue_Marker%s.png".format( letter)
                        
                        Some( <pic lon={pic.coord.lon.toString} lat={pic.coord.lat.toString} img={imageUrl} link={link} title={title} author={authorName} icon={letterLink}/> )
                    }
                    catch
                    {
                        case _ : Throwable => None
                    }
                }
                .flatten
            }
            </pics>
        </gpx>
    }
    
    def template( pageName : String, onBodyLoad : Option[String] = None )( sideBarLeft : scala.xml.Elem )( pageBody : scala.xml.Elem )( sideBarRight : scala.xml.Elem ) =
    {
        contentType = "text/html"
        val page = 
            <html>
                <head>
                    <title>{pageName}</title>
                    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                    
                    <link href="css/bootstrap.min.css" rel="stylesheet" media="screen"/>
                    <link href="css/bootstrap-responsive.min.css" rel="stylesheet"/>
                    
                    <script src="http://www.openlayers.org/api/OpenLayers.js"></script>
                    <script src="http://www.openstreetmap.org/openlayers/OpenStreetMap.js"></script>
                    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"></script>
                    <script src="/js/osmmap.js"></script>
                    

                    <style>
                      body {{
                        padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
                        padding-left: 10px;
                        padding-right: 10px;
                      }}
                    </style>
                    
                </head>
                <body onLoad={onBodyLoad.map( s => scala.xml.Text(s) )} style="height:90%">

                    <div class="navbar navbar-fixed-top">
                      <div class="navbar-inner">
                        <div class="row-fluid"><div class="span12" style="padding-left: 10px">
                          <a class="brand" href="/">Routility</a>
                          <div class="nav-collapse collapse">
                            <ul class="nav">
                              <li class="active"><a href="/"><strong>Home</strong></a></li>
                              <li><a href="/displayroute">Plan Route</a></li>
                              <li><a href="/login">Login</a></li>
                            </ul>
                          </div><!--/.nav-collapse -->
                          
                            
                            <div style="display: inline">
                                {
                                    val clientState = "1234"
                                    val redirectURI="http://www.two-pi.co.uk/googleoauth2callback"
                                    val googleClientId = "725550604793.apps.googleusercontent.com"
                                    val googleOpenIdLink="https://accounts.google.com/o/oauth2/auth?scope=%s&state=%s&response_type=token&client_id=%s&redirect_uri=%s".format(
                                        "https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile",
                                        clientState,
                                        googleClientId,
                                        redirectURI )
                                        
                                    <a class="btn" href={googleOpenIdLink}>
                                        <img src="/img/G.png" alt="log in with Google" style="height: 1.2em"/>
                                    </a>
                                }
                            </div>
                          
                        </div></div>
                      </div>
                    </div>

                    <div class="row-fluid">

                        <div class="span2" style="height:90%">
                        {
                            sideBarLeft
                        }
                        </div>
                        
                        <div class="span8" style="height:90%; overflow: hidden">
                        {
                            pageBody
                        }
                        </div>
                        
                        <div class="span2" style="height:90%; overflow-y: auto; overflow-x: hidden">
                        {
                            sideBarRight
                        }
                        </div>

                    </div>
                  </body>
            </html>
            
        val pp = new scala.xml.PrettyPrinter( 100, 2 )
        val res = pp.format( page )
        //"<!DOCTYPE html>\n" + res
        res
    }


    // So what do we do with this information?
    // http://www.jaredarmstrong.name/2011/08/scalatra-an-example-authentication-app/ and 
    // http://www.jaredarmstrong.name/2011/08/scalatra-form-authentication-with-remember-me/
    get("/googleoauth2callback")
    {
        // Google etc incoming.
    }

    get("/")
    {
        template("Routility")
        {
            <h3>Sidebar</h3>
        }
        {
            <div>
                <h1>Main page</h1>
                
                <a href="/displayroute">Navigate to navigate.</a>
            </div>
        }
        {
            <h3>Sidebar</h3>
        }
    }
    
  
    
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
        
        "http://s%d.geograph.org.uk%s_213x160.jpg".format( index % 4, fullPath )
    }
    
    
    get("/displayroute")
    {
        val lon = params.getOrElse("lon","-1.361461").toDouble
        val lat = params.getOrElse("lat", "51.709").toDouble
        val distInKm = params.getOrElse("distance", "30.0").toDouble
        val seed = params.getOrElse("seed", "1").toInt
        
        val onLoad = "init( %f, %f, 12, '/route?lon=%f&lat=%f&distance=%f&seed=%d' );".format( lon, lat, lon, lat, distInKm, seed )
        
        val xmlData = getRouteXML( lon, lat, distInKm, seed )
        
        template( "Plan a trip", onBodyLoad=Some(onLoad) )
        {
            <div>
                <h3>Actions</h3>
                <form action="/displayroute" method="get">
                    <fieldset>
                        <label for="lon">Longitude</label>
                        <input class="input-medium" name="lon" id="lon" type="text" value={lon.toString}></input>
                        
                        <label for="lat">Latitude</label>
                        <input class="input-medium" name="lat" id="lat" type="text" value={lat.toString}></input>
                        
                        <label for="distance">Distance (km)</label>
                        <input class="input-medium" name="distance" type="text" value={distInKm.toString}></input>
                        
                        <label for="seed">Seed</label>
                        <input class="input-medium" name="seed" type="text" value={(seed+1).toString}></input>
                        
                        <label for="routeType">Route type</label>
                        <select id="routeType">
                            <option>Walking, flat</option>
                            <option>Walking, hilly</option>
                            <option>Cycling, flat</option>
                            <option>Cycling, hilly</option>
                        </select>
                        
                        <br/>
                        
                        <button type="submit" class="btn btn-primary">Generate route</button>
                    </fieldset>
                </form>
            </div>
        }
        {
            <div>
                <!-- define a DIV into which the map will appear. Make it take up the whole window -->
                <!--<div style="width:100%; height:80%" id="map"></div>-->
                <div id="map"></div>
                
                <!-- Add elevation chart at bottom from highcharts, e.g. http://www.highcharts.com/demo/line-ajax -->
                <div style="width:100%; height:20%" id="elevation"></div>
                
            </div>
        }
        {
            val gpxUrl = "/routegpx?lon=%f&lat=%f&distance=%f&seed=%d".format( lon, lat, distInKm, seed )
            val fullRouteUrl = "/route?lon=%f&lat=%f&distance=%f&seed=%d".format( lon, lat, distInKm, seed )
            
            <div>
                <h3>Route</h3>
                <div>
                    <div class="btn-group text-center">
                        <a href={gpxUrl} class="btn">GPX</a>
                        <a href={fullRouteUrl} class="btn">Full route</a>
                    </div>
                    
                    <hr/>
                    
                    {
                        val pics = xmlData \\ "pic"
                        pics.zip(letters)map
                        { case (p, l) =>

                            val fullLink = (p \ "@link").text
                            val imageUrl = (p \ "@img").text
                            val title = l + ": " + (p \ "@title").text
                            val credits = "Copyright %s and licensed for reuse under the Creative Commons Licence.".format( (p \ "@author").text )
                            
                            <a href={fullLink}><img src={imageUrl} alt={credits} title={credits}/></a>
                            <br/>
                            <div>{title}</div>
                            <br/>
                        }
                    }
                </div>
            </div>
        }
    }
    
    // Embed route data in page in <script id="foo" type="text/xmldata"> tag?
    get("/route")
    {
        contentType="text/xml"
        
        val rgh = getRGH
        
        val lon = params("lon").toDouble
        val lat = params("lat").toDouble
        val distInKm = params("distance").toDouble
        val seed = params("seed").toInt
        
        getRouteXML( lon, lat, distInKm, seed )
    }
    
    get("/routegpx")
    {
        contentType="text/xml"
        
        val rgh = getRGH
        
        val lon = params("lon").toDouble
        val lat = params("lat").toDouble
        val distInKm = params("distance").toDouble
        val seed = params("seed").toInt
        
        val routeXML = getRouteXML( lon, lat, distInKm, seed )
        
        // Extract only the trk bit (the rest isn't valid gpx)
        <gpx>
        {
            routeXML \ "trk"
        }
        </gpx>
    }
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

