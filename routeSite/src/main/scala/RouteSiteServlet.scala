package org.seacourt.routeSite

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.{OSMMap, Coord}
import org.seacourt.osm.route.RoutableGraph

// In sbt:
//
// > container:start
// > ~ ;copy-resources;aux-compile

class RouteGraphHolder
{
    val mapFile = new java.io.File( "/home/alex.wilson/Devel/AW/OSM-Crunch/oxfordshire.bin" )    
    val map = OSMMap.load( mapFile )
    val rg = RoutableGraph( map )
}

class RouteSiteServlet extends ScalatraServlet with ScalateSupport
{
    var rghOption : Option[RouteGraphHolder] = None
    
    def template( pageName : String )( pageBody : scala.xml.Elem ) =
    {
        <html>
            <head>
                <title>{pageName}</title>
                <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                
                <link href="css/bootstrap.min.css" rel="stylesheet" media="screen"/>
                <link href="css/bootstrap-responsive.css" rel="stylesheet"/>
                <style>
                  body {{
                    padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
                  }}
                </style>
            </head>
            <body>

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

                <div class="container">

                  {
                    pageBody
                  }

                </div> <!-- /container -->

                <!-- Le javascript
                ================================================== -->
                <!-- Placed at the end of the document so the pages load faster -->
                <script src="../assets/js/jquery.js"></script>
                <script src="../assets/js/bootstrap-transition.js"></script>
                <script src="../assets/js/bootstrap-alert.js"></script>
                <script src="../assets/js/bootstrap-modal.js"></script>
                <script src="../assets/js/bootstrap-dropdown.js"></script>
                <script src="../assets/js/bootstrap-scrollspy.js"></script>
                <script src="../assets/js/bootstrap-tab.js"></script>
                <script src="../assets/js/bootstrap-tooltip.js"></script>
                <script src="../assets/js/bootstrap-popover.js"></script>
                <script src="../assets/js/bootstrap-button.js"></script>
                <script src="../assets/js/bootstrap-collapse.js"></script>
                <script src="../assets/js/bootstrap-carousel.js"></script>
                <script src="../assets/js/bootstrap-typeahead.js"></script>

              </body>
        </html>
    }

    get("/")
    {
        template("Routility")
        {
            <div>
                <h1>Main page</h1>
                
                Say <a href="hello-scalate">hello</a> to me.
            </div>
        }
    }
  
    get("/hello-scalate")
    {
        template("Thank-you!")
        {
            <div>
                <h1>Why, thank-you.</h1>
            </div>
        }
    }
    
    // e.g. http://localhost:8080/route?data=-1.3611464,51.7094267,50.0,3
    get("/route")
    {
        if ( rghOption.isEmpty ) rghOption = Some( new RouteGraphHolder() )
        
        val rgh = rghOption.get

        val els = params("data").split(",")
        
        val lon = els(0).toDouble
        val lat = els(1).toDouble
        val distInKm = els(2).toDouble
        val seed = els(3).toInt
        
        val startCoords = Coord(lon, lat)
        
        println( "Finding closest node..." )
        val closestNode = rgh.rg.getClosest( startCoords )
        
        println( "Closest: " + closestNode.node.coord )
        
        val routeNodes = rgh.rg.buildRoute( closestNode, distInKm * 1000.0, seed )
        
        routeNodes.map
        { rn =>
            "%f,%f".format( rn.node.coord.lat, rn.node.coord.lon )
        }.mkString("\n")
    }
}

