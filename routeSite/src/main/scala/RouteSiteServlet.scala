package org.seacourt.routeSite

import scala.collection.{mutable, immutable}

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Coord, Logging}
import org.seacourt.osm.route.{RoutableGraph, RoutableGraphBuilder, RouteNode, RTreeIndex, ScenicPoint}

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
    val rg = RoutableGraphBuilder.load( new java.io.File( "./default.bin.rg" ) )
}
// Weird cost:
// http://localhost:8080/displayroute?lon=-3.261151337280192&lat=54.45527013007099&distance=30.0&seed=1
class RouteSiteServlet extends ScalatraServlet with ScalateSupport with FlashMapSupport with Logging
{
    import net.sf.ehcache.{CacheManager, Element}
    
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
    
    val letters = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    
    private def getRouteXML( lon : Double, lat : Double, distInKm : Double ) = //cached[scala.xml.Node]("routeXML", lon, lat, distInKm )
    {
        import net.liftweb.json.{JsonParser, DefaultFormats, JObject}
        import scalaj.http.{Http, HttpOptions}
        implicit val formats = DefaultFormats
        
        val rgh = getRGH
        val startCoords = Coord(lon, lat)
        
        log.info( "Finding closest node..." )
        val closestNode = rgh.rg.getClosest( startCoords )
        
        log.info( "Closest: " + closestNode.coord )
        
        rgh.rg.buildRoute( closestNode, distInKm * 1000.0 ).map
        { route =>
        
            val routeNodes = route.routeNodes
            val pics = route.picList
            
            var lastRN : Option[RouteNode] = None
            
            // TODO: Distance calculations here are inaccurate - need to use the edge distance,
            // not the node-to-node distance. Return the correct data from buildRoute!
            val distance = routeNodes
                .sliding(2)
                .map { case Seq( rn1, rn2 ) => rn2.coord.distFrom( rn1.coord ) }
                .sum / 1000.0

            val ascent = routeNodes
                .sliding(2)
                .map { case Seq( rn1, rn2 ) => rn2.height - rn1.height }
                .filter( _ > 0.0 )
                .sum
            
            var cumDistance = 0.0
            <route>
                <metadata>
                    <request>
                        <lon>{lon}</lon>
                        <lat>{lat}</lat>
                        <distance>{distInKm}</distance>
                    </request>
                    <summary>
                        <distance>{distance}</distance>
                        <ascent>{ascent}</ascent>
                    </summary>
                </metadata>
                
                <pois>
                {
                    route.pois.map
                    { p =>
                        val linkOption = (p.wikiData.map(wd => xml.Text("http://en.wikipedia.org/wiki/" + wd.name)))
                        
                        <poi name={p.name} link={linkOption} lon={p.coord.lon.toString} lat={p.coord.lat.toString} icon={p.poiType.icon.toString}/>
                    }
                }
                </pois>
                
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
                </gpx>
            </route>
        }
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
                    <script src="http://code.highcharts.com/highcharts.js"></script>
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
                          <a class="brand" href="/">TwoPI.co.uk</a>
                          <div class="nav-collapse collapse">
                            <ul class="nav">
                              <li class="active"><a href="/"><strong>Home</strong></a></li>
                              <li><a href="/">Plan Route</a></li>
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
                    
                    {
                        flash.get("error") match
                        {
                            case Some(message) =>
                                <div class="row-fluid">
                                    <div class="span12">
                                        <div style="background-color: red; color:white">
                                            <p class="text-center"><strong>Error: {message}</strong></p>
                                        </div>
                                    </div>
                                </div>
                            case None => <div/>
                        }
                    }

                    <div class="row-fluid">

                        <div class="span2" style="height:95%">
                        {
                            sideBarLeft
                        }
                        </div>
                        
                        <div class="span8" style="height:95%; overflow: hidden">
                        {
                            pageBody
                        }
                        </div>
                        
                        <div class="span2" style="height:95%; overflow-y: auto; overflow-x: hidden">
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
    // Auth flow example: oauthssodemo.appspot.com/step/1
    
    // http://www.jaredarmstrong.name/2011/08/scalatra-an-example-authentication-app/ and 
    // http://www.jaredarmstrong.name/2011/08/scalatra-form-authentication-with-remember-me/
    get("/googleoauth2callback")
    {
        // Google etc incoming, reply of form:
        //http://www.two-pi.co.uk/googleoauth2callback#state=1234&access_token=ya29.AHES6ZQDbSARS6qaTmJJ_G7FQXBgSEwP8TJWc0OwDxvgl7V7ZRQrKQ&token_type=Bearer&expires_in=3600
        log.info( "Callback from google" )
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
    
    def routeFilePath( routeId : String ) = new java.io.File("routes/route_%s.xml".format(routeId)).getAbsoluteFile
    
    def messageDigest( s : String ) = java.security.MessageDigest
        .getInstance("MD5")
        .digest(s.getBytes)
        .map("%02X".format(_))
        .mkString
    
    post("/makeroute")
    {
        val lon = params("lon").toDouble
        val lat = params("lat").toDouble
        val distInKm = params("distance").toDouble
        
        println( lon, lat, distInKm )
        
        getRouteXML( lon, lat, distInKm ) match
        {
            case Some( routeXml ) =>
            {
                val hash = messageDigest( routeXml.toString )
                if (false)
                {
                    // Currently this appears to crash saving XML in ways I do not understand
                    scala.xml.XML.save( routeFilePath( hash ).toString, routeXml )
                }
                else
                {
                    val res = routeXml.toString
                    
                    val pw = new java.io.PrintWriter( routeFilePath( hash ) )
                    pw.print( res )
                    pw.close
                }
                
                redirect( "/?routeId=%s".format(hash) )
            }
            case None =>
            {
                flash("error") = "Could not find a route to your specification. Please try modifying length or start point."
                
                redirect( "/" )
            }
        }
    }
    
    
    // Embed route data in page in <script id="foo" type="text/xmldata"> tag?
    get("/route/:routeId")
    {
        contentType="text/xml"
        
        val routeId = params("routeId")
        scala.xml.XML.loadFile( routeFilePath( routeId ) )
    }
    
    case class RouteData( routeId : String, lon : Double, lat : Double, requestDist : Double, distance : Double, ascent : Double, distHeightSeries : Seq[(Double, Double)], xmlData : scala.xml.Node )
    
    get("/")
    {
        val routeIdOption = params.get("routeId")
        
        val routeDataOption = routeIdOption.map
        { case routeId =>
        
            val xmlData = scala.xml.XML.loadFile( routeFilePath( routeId ) )
            
            val routeMetadata = xmlData \ "metadata"
            val routeRequest = routeMetadata \ "request"
            
            val lon = (routeRequest \ "lon").text.toDouble
            val lat = (routeRequest \ "lat").text.toDouble
            val requestDistInKm = (routeRequest \ "distance").text.toDouble
            
            val routeSummary = routeMetadata \ "summary"
            val distance = (routeSummary \ "distance").text.toDouble
            val ascent = (routeSummary \ "ascent").text.toDouble
            
            val distHeightSeries : Seq[(Double, Double)] = (xmlData \\ "trkpt").map( el => ((el \ "@distance").text.toDouble / 1000.0, (el \ "@ele").text.toDouble) )
            
            RouteData( routeId, lon, lat, requestDistInKm, distance, ascent, distHeightSeries, xmlData )
        }
        
        val onLoad = routeDataOption match
        {
            case Some(rd)   => "init( %f, %f, 12, '/route/%s', '/routegpx/%s' );".format( rd.lon, rd.lat, rd.routeId, rd.routeId )
            case None       => "initDefault();"
        }
        
        template( "Plan a trip", onBodyLoad=Some(onLoad) )
        {
            <div>
                <h3>Actions</h3>
                <form action="/makeroute" method="post">
                    <fieldset>
                        <input name="lon" id="lon" type="hidden" value={routeDataOption.map(_.lon.toString).getOrElse("")}></input>
                        <input name="lat" id="lat" type="hidden" value={routeDataOption.map(_.lat.toString).getOrElse("")}></input>
                        
                        <label for="distance">Distance (km)</label>
                        <input class="input-medium" name="distance" type="text" value={routeDataOption.map(_.requestDist.toString).getOrElse("")}></input>
                        
                        <label for="routeType">Route type</label>
                        <select id="routeType">
                            <option>Walking</option>
                            <!--<option>Walking, hilly</option>
                            <option>Cycling, flat</option>
                            <option>Cycling, hilly</option>-->
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
                <div style="height:80%" id="map"></div>
                
                <!-- Add elevation chart at bottom from highcharts -->
                <div style="height:20%" id="elevation">
                </div>
                <script>
                {
                    val distHeightSeries = routeDataOption.map( _.distHeightSeries ).getOrElse( Seq() )
                    
                    val seriesString = "[" + distHeightSeries.map( x => "[%f, %f]".format( x._1, x._2 ) ).mkString( ", " ) + "]"
                    xml.Unparsed("""
                        $(function() {
                            $('#elevation').highcharts({
                                chart : { type : 'line' },
                                title : { text : 'Elevation profile' },
                                xAxis : { title : { text : 'Distance' } },
                                yAxis : { title : { text : '(m)' } },
                                series : [{ showInLegend: false, name : 'elevation', type : 'area', data : %s }],
                                plotOptions : { series : { marker : { enabled : false } } }
                            });
                        });
                    """.format(seriesString))
                }
                </script>
                
            </div>
        }
        {
            
            
            <div>
                <h3>Route</h3>
                {
                    routeDataOption match
                    {
                        case Some(rd) =>
                        {
                            val gpxUrl = "/routegpx/%s".format( rd.routeId )
                            val fullRouteUrl = "/route/%s".format( rd.routeId )
                        
                            <div>
                                
                                <strong>Distance:</strong> { "%.2f km".format(rd.distance) }
                                <br/>
                                <strong>Ascent:</strong> { "%d m".format(rd.ascent.toInt) }
                                
                                <br/><br/>

                                <div class="btn-group text-center">
                                    <a href={gpxUrl} class="btn">GPX</a>
                                    <a href={fullRouteUrl} class="btn">Full route</a>
                                </div>
                                
                                <hr/>
                                
                                {
                                    val pics = rd.xmlData \\ "pic"
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
                        }
                        case None => <div/>
                    }
                }
            </div>
        }
    }
    
    
    
    get("/routegpx/:routeId")
    {
        contentType="text/xml"
        
        val routeId = params("routeId")
        val routeXML = scala.xml.XML.loadFile( routeFilePath( routeId ) )
        
        // Extract only the trk bit (the rest isn't valid gpx)
        routeXML \ "gpx"
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

