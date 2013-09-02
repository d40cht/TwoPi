package org.seacourt.routeSite

import scala.collection.{mutable, immutable}

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Coord}
import org.seacourt.osm.route.{RoutableGraph, RouteNode}

// In sbt:
//
// > container:start
// > ~ ;copy-resources;aux-compile

class RouteGraphHolder
{
    val mapFile = new java.io.File( "./uk.bin" )
    //val mapFile = new java.io.File( "./oxfordshire.bin" )
    val map = OSMMap.load( mapFile )
    val rg = RoutableGraph( map )
}
// Weird cost:
// http://localhost:8080/displayroute?lon=-3.261151337280192&lat=54.45527013007099&distance=30.0&seed=1
class RouteSiteServlet extends ScalatraServlet with ScalateSupport
{
    import net.sf.ehcache.{CacheManager, Element}

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
            
            if ( cache.isKeyInCache(hash) )
            {
                println( "Cached element found" )
                val el = cache.get(hash)
                el.getObjectValue.asInstanceOf[T]
            }
            else
            {
                println( "Cached element not found - running function" )
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
    
    private def getRouteXML( lon : Double, lat : Double, distInKm : Double, seed : Int ) = cached[scala.xml.Node]("routeXML", lon, lat, distInKm, seed )
    {
        import net.liftweb.json.{JsonParser, DefaultFormats, JObject}
        import scalaj.http.Http
        implicit val formats = DefaultFormats
        
        val rgh = getRGH
        val startCoords = Coord(lon, lat)
        
        println( "Finding closest node..." )
        val closestNode = rgh.rg.getClosest( startCoords )
        
        println( "Closest: " + closestNode.node.coord )
        
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
                            case Some( lrn ) => lrn.node.coord.distFrom( rn.node.coord )
                            case None => 0.0
                        }
                        cumDistance += dist
                        
                        val res = <trkpt lat={rn.node.coord.lat.toString} lon={rn.node.coord.lon.toString} distance={cumDistance.toString}/>
                        lastRN = Some( rn )
                        res
                    }
                }
                </trkseg>
            </trk>
            <pics>
            {
                pics.map
                { pic =>

                    val fullLink = pic.link
                    val picIndex = fullLink.split("/").last.toInt
                    
                    val resJSON = Http("http://jam.geograph.org.uk/sample8.php?q=&select=title,grid_reference,realname,user_id,hash&range=%d,%d".format( picIndex, picIndex ))
                    { inputStream => 
                        JsonParser.parse(new java.io.InputStreamReader(inputStream))
                    }
                    
                    val imgMatches = (resJSON \\ "matches")
                    val imgMetaData = imgMatches.asInstanceOf[JObject].obj.head.value
    
                    val title = (imgMetaData \\ "title").extract[String]
                    val authorName = (imgMetaData \\ "realname").extract[String]
                    val hash = (imgMetaData \\ "hash").extract[String]
                    
                    val imageUrl = imgUrl( picIndex, hash )
                    
                    <pic lon={pic.coord.lon.toString} lat={pic.coord.lat.toString} img={imageUrl} link={pic.link} title={title} author={authorName}/>
                }
            }
            </pics>
        </gpx>
    }
    
    def template( pageName : String, onBodyLoad : Option[String] = None )( sideBar : scala.xml.Elem )( pageBody : scala.xml.Elem ) =
    {
        <html>
            <head>
                <title>{pageName}</title>
                <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                
                <link href="css/bootstrap.min.css" rel="stylesheet" media="screen"/>
                <link href="css/bootstrap-responsive.css" rel="stylesheet"/>
                
                <script src="http://www.openlayers.org/api/OpenLayers.js"></script>
                <script src="http://www.openstreetmap.org/openlayers/OpenStreetMap.js"></script>
                <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"></script>
                <script src="/js/osmmap.js"></script>

                <style>
                  body {{
                    padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
                  }}
                </style>
                
            </head>
            <body onLoad={onBodyLoad.map( s => scala.xml.Text(s) )}>

                <div class="navbar navbar-inverse navbar-fixed-top">
                  <div class="navbar-inner">
                    <div class="container">
                      <button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
                        <span class="icon-bar"></span>
                        <span class="icon-bar"></span>
                        <span class="icon-bar"></span>
                      </button>
                      <a class="brand" href="/">Routility</a>
                      <div class="nav-collapse collapse">
                        <ul class="nav">
                          <li class="active"><a href="/">Home</a></li>
                          <li><a href="/about">About</a></li>
                          <li><a href="/contact">Contact</a></li>
                        </ul>
                      </div><!--/.nav-collapse -->
                    </div>
                  </div>
                </div>

                <div class="container-fluid">

                    <div class="span2">
                    {
                        sideBar
                    }
                    </div>
                    
                    <div class="span10">
                    {
                        pageBody
                    }
                    </div>

                </div>
              </body>
        </html>
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
    }
  
    get("/hello-scalate")
    {
        template("Thank-you!")
        {
            <h3>Sidebar</h3>
        }
        {
            <div>
                <h1>Why, thank-you.</h1>
            </div>
        }
    }
    
    private def imgUrl( index : Int, hash : String ) : String =
    {
        val yz = index / 1000000
        val ab = (index % 1000000) / 10000
        val cd = (index % 10000) / 100
        
        if ( yz == 0 )
        {
            "http:///s0.geograph.org.uk/photos/%02d/%02d/%06d_%s.jpg".format( ab, cd, index, hash )
        }
        else
        {
            "http:///s0.geograph.org.uk/geophotos/%02d/%02d/%02d/%06d_%s.jpg".format( yz, ab, cd, index, hash )
        }
    }
    
    
    get("/displayroute")
    {
        val lon = params.getOrElse("lon","-1.361461").toDouble
        val lat = params.getOrElse("lat", "51.709").toDouble
        val distInKm = params.getOrElse("distance", "30.0").toDouble
        val seed = params.getOrElse("seed", "1").toInt
        
        val onLoad = "init( %f, %f, 12, '/route?lon=%f&lat=%f&distance=%f&seed=%d' );".format( lon, lat, lon, lat, distInKm, seed )
        
        val xmlData = getRouteXML( lon, lat, distInKm, seed )
        
        template( "Display route", onBodyLoad=Some(onLoad) )
        {
            <div>
                {
                    val pics = xmlData \\ "pic"
                    pics.map
                    { p =>

                        val fullLink = (p \ "@link").text
                        val imageUrl = (p \ "@img").text
                        val title = (p \ "@title").text
                        val credits = "Copyright %s and licensed for reuse under the Creative Commons Licence.".format( (p \ "@author").text )
                        
                        <a href={fullLink}><img src={imageUrl} alt={credits} title={credits}/></a>
                        <br/>
                        <div>{title}</div>
                        <br/>
                    }
                }
            </div>
        }
        {
            <div>
                <!-- define a DIV into which the map will appear. Make it take up the whole window -->
                <div style="width:100%; height:80%" id="map"></div>
                <div style="text-align:center">
                    <form action="/displayroute" method="get">
                        Longitude: <input name="lon" id="lon" type="text" value={lon.toString}></input>
                        Latitude: <input name="lat" id="lat" type="text" value={lat.toString}></input>
                        Distance (km): <input name="distance" type="text" value={distInKm.toString}></input>
                        Seed: <input name="seed" type="text" value={(seed+1).toString}></input>
                        <input type="submit" value="Generate route"/>
                    </form>
                </div>
            </div>
        }
    }
    
    
    // Render to: http://www.darrinward.com/lat-long/
    // e.g. http://localhost:8080/route?data=-1.3611464,51.7094267,50.0,3
    
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
}

