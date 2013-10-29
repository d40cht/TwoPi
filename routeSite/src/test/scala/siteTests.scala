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

import org.seacourt.osm.Coord
import org.seacourt.osm.route.RouteResult



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
    
    
    private def shortEventually( fn : => Unit ) = eventually( timeout(Span(2, Seconds) ) )(fn)
    private def longEventually( fn : => Unit ) = eventually( timeout(Span(10, Seconds) ) )(fn)
    
    override def beforeAll()
    {
        sys.props.put("withInMemory", "true")
        startJetty()
        
        // Wait a bit for the route graph to load
        Thread.sleep(2000)
    }

    override def afterAll()
    {
        // Close the web page
        close()
        sys.props.remove("withInMemory")
        stopJetty()
    }
    
    val summerTownCoords = "-1.2657,51.777"
    
    "A web server" should "appear on port 8080 be testable, including dynamic content" in
    {
        // TODO: Must use mock persistence
    	//val persistence = new MockPersistence
        
        go to (testRootUrl + "/app")
        reloadPage()
        reloadPage()
        
        pageTitle should be ("TwoPI.co.uk: Circular walking and cycling routes")
        
        shortEventually
        {
            val controlBar = find( className("controlBar") )
            assert( controlBar.size === 1 )
        }
    }
    
    
    
    "A web server" should "allow logging on and off and remember users" in
    {
    	go to (testRootUrl + "/app")
        
    	click on id("guestLogon")
    	shortEventually
    	{
    	    assert( find(id("flashInfo")).get.text contains "Thanks for joining: A guest" )
        }
    	
    	reloadPage()
    	assert( find(id("flashInfo")) === None )
    	
    	click on linkText("A guest")
    	assert( find(id("wrap")).get.text contains "My routes" )
    	
    	click on linkText("TwoPi.co.uk")
    	
    	shortEventually
    	{
    	    assert( find( className("controlBar") ) != None )
        }
    	
    	click on linkText("Logout")
    	shortEventually
    	{
    	    assert( find(id("flashInfo")).get.text contains "Goodbye: A guest" )
        }
    	
    	click on id("guestLogon")
    	shortEventually
    	{
    	    assert( find(id("flashInfo")).get.text contains "Welcome back: A guest" )
        }
    	
    	// Setting the route name should be disabled because there is no route
    	assert( !find(id("routeName")).get.isEnabled )
    }
    
    "The server" should "provide async place search functionality" in
    {
        
        textField("placeSearchInput").value = "Nether wasdale"
        click on "placeSearchSubmit"
        
        Thread.sleep(1000)
        
        // Check that place search works
        longEventually
        {
            val searchResults = find("placeSearchResults").get
            assert( searchResults.text contains "Cumbria" )
        }
    }
    
    "The server" should "support route generation without failing" in
    {      
        // Check that generating a route works
        click on "startCoord"
        enter( summerTownCoords )
        click on "distance"
        enter( "20" )
        
        click on "specifiedStartSubmit"
        Thread.sleep(2000)
        
        // Wait for the route to arrive
        longEventually
        {
            val routePreference = find("routePreference").get
            assert( routePreference.text === "Walking" )
        }
    }

    "The server" should "render and update the UI appropriately" in
    {
        longEventually
        {
            val routePreference = find("routePreference").get
            assert( routePreference.text === "Walking" )
        }
        
        // Setting the route name should be disabled because we are not logged on 
        assert( !find(id("routeName")).get.isEnabled )
    }
    
    "Once logged on it" should "be possible to save routes" in
    {
        go to (testRootUrl + "/app")
        click on "startCoord"
        enter( summerTownCoords )
        click on "distance"
        enter( "20" )
        click on "specifiedStartSubmit"
        
        Thread.sleep(2000)
        
        // It should now be possible to set the route name
        longEventually
        {
            assert( find(id("routeName")).get.isEnabled )
        }
        
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
    
    def addRoute( routeData : String, start : Coord, routeType : String, distance : Double, ascent : Double, duration : Double, userId : Option[Int] ) : Int = ???
    def getRoute(routeId : Int): Option[RouteResult] = ???
    def getRouteSummary( routeId : Int ) : Option[RouteSummary] = ???
    def getRouteName( routeId : Int ) : Option[RouteName] = ???
    def getUserRoutes(userId : Int): List[UserRoute] = ???
    def nameRoute(userId: Int, routeId: Int, routeName : String, description : String): Unit = ???

    
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




