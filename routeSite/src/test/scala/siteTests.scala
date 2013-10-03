import org.scalatra._
import org.scalatra.test.scalatest._
import org.seacourt.routeSite._
import org.scalatest.{ShouldMatchers, BeforeAndAfter, BeforeAndAfterAll, FlatSpec}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import javax.servlet.ServletContext
import org.openqa.selenium.firefox.FirefoxDriver
import org.scalatest.selenium._
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.Eventually.Interval
import org.scalatest.time.Span
import org.scalatest.time.Seconds




trait EmbeddedJetty
{
    protected var jetty: Server = null
    protected var context: ServletContext = null
    
    val testPort = 8080
    val testRootUrl = "http://localhost:" + testPort

    def startJetty()
    {
        jetty = new Server(testPort)
        jetty setHandler prepareContext
        jetty.start()
    }

    private def prepareContext() =
    {
        val context = new WebAppContext()
        context setContextPath "/"
        val resourceBase = getClass.getClassLoader.getResource("webapp").toExternalForm
        context setResourceBase resourceBase
        this.context = context.getServletContext
        context
    }


    def stopJetty() 
    {
        jetty.stop()
    }
}



class SeleniumTest extends FlatSpec with ShouldMatchers with servlet.ServletApiImplicits with EmbeddedJetty with BeforeAndAfterAll with BeforeAndAfter with Eventually with Firefox
{
    //implicit val webDriver : WebDriver = new FireFoxDriver
    
    
    
    override def beforeAll()
    {
        sys.props.put("withInMemory", "true")
        startJetty()
    }

    override def afterAll()
    {
        // Close the web page
        close()
        sys.props.remove("withInMemory")
        stopJetty()
    }
    
    "A web server" should "appear on port 8080 and be testable" in
    {
        // TODO: Must use mock persistence
    	//val persistence = new MockPersistence
        
        go to (testRootUrl + "/app")
        
        pageTitle should be ("TwoPI.co.uk: Circular walking and cycling routes")
        
        val controlBar = find( className("controlBar") )
        assert( controlBar.size === 1 )
        
        textField("placeSearchInput").value = "Nether wasdale"
        click on "placeSearchSubmit"
        
        // Check that place search works
        eventually( timeout(Span(2, Seconds)) )
        {
            val searchResults = find("placeSearchResults").get
            assert( searchResults.text contains "Cumbria" )
        }
        
        // Check that generating a route works
        click on "startCoord"
        enter( "-1.312001,51.77463" )
        click on "distance"
        enter( "20" )
        click on "specifiedStartSubmit"
        
        Thread.sleep(6000)
        
        eventually( timeout(Span(6, Seconds)) )
        {
            val routePreference = find("routePreference").get
            routePreference.text == "Walking"
        }
    }
    
    "A web server" should "allow logging on and off and remember users" in
    {
    	go to (testRootUrl + "/app")
        
    	click on id("guestLogon")
    	assert( find(id("flashInfo")).get.text contains "Thanks for joining: A guest" )
    	
    	reloadPage()
    	assert( find(id("flashInfo")) === None )
    	
    	click on linkText("A guest")
    	assert( find(id("wrap")).get.text contains "My routes" )
    	
    	click on linkText("TwoPi.co.uk")
    	assert( find( className("controlBar") ) != None )
    	
    	click on linkText("Logout")
    	assert( find(id("flashInfo")).get.text contains "Goodbye: A guest" )
    	
    	click on id("guestLogon")
    	assert( find(id("flashInfo")).get.text contains "Welcome back: A guest" )
    }
    
}



case class MockPersistence() extends Persistence
{
    def getUser( extId : String ) : Option[User] =
    {
        val uo = users.get(extId)
        
        uo foreach
        { u =>
        
            val now = new java.sql.Timestamp( (new java.util.Date()).getTime() )
            users += (extId -> u.copy( numLogins=u.numLogins + 1, lastLogin=now ))
        }
        
        uo
    }
    
    def addUser( extId : String, email : String, name : String ) : User =
    {
        val now = new java.sql.Timestamp( (new java.util.Date()).getTime() )
        val nu = new User( users.size, extId, name, email, 0, now, now )
        
        users += (extId -> nu)
        
        nu
    }
    
    def addRoute( routeJSON : String, distance : Double, ascent : Double ) : Int = ???
    def getRoute(routeId: Int): Option[String] = ???
    def getUserRoutes(userId: Int): List[UserRoute] = ???
    def saveRouteToUser(userId: Int,routeId: Int,routeName: String): Unit = ???

    
    private var users = Map[String, User]()
}



class WebServicesTest extends FlatSpec with ScalatraSuite
{
    val persistence = new MockPersistence
    addServlet( new RouteSiteServlet(persistence), "/*" )
    
    "RouteSiteServlet" should "redirect to the app" in
    {
        get("/")
        {
            status should equal (302)
        }
    }
     
    "RouteSiteServlet" should "render the front page with no sign-in" in
    {   
        get("/app")
        {
            status should equal (200)
            body should include ("TwoPi.co.uk")
            body should include ("Sign in")
        }
    }
}


