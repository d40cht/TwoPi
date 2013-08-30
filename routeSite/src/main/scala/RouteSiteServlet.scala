package org.seacourt.routeSite

import org.scalatra._
import scalate.ScalateSupport

// In sbt:
//
// > container:start
// > ~ ;copy-resources;aux-compile

class RouteSiteServlet extends ScalatraServlet with ScalateSupport {

    get("/")
    {
        <html>
            <body>
                <h1>Hello, world!</h1>
                Say <a href="hello-scalate">hello to Scalate</a>.
            </body>
        </html>
    }
  
    get("/hello-scalate")
    {
        <html>
            <body> 
                <h1>Why, thank-you</h1>
            </body>
        </html>
    }
}

