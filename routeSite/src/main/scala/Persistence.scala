package org.seacourt.routeSite

import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.driver.H2Driver.simple._

import java.sql.{Timestamp}

import org.seacourt.osm.Coord
import org.seacourt.osm.route.{POIType, RouteResult}

// JSON handling support
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import org.json4s.native.Serialization.{read => sread, write => swrite}

case class User( id : Int, extId : String, name : String, email : String, numLogins : Int, firstLogin : Timestamp, lastLogin : Timestamp )
case class UserRoute( id : Int, name : String, description : String, startCoord : Coord, distance : Double, ascent : Double, timeAdded : Timestamp )

case class RouteName( name : String, description : String, timeAdded : Timestamp )

case class NamedRoute( name : Option[RouteName], route : RouteResult )


private object UserTable extends Table[User]("Users")
{
    def id          = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def extId       = column[String]("extId")
    def name        = column[String]("name")
    def email       = column[String]("email")
    def numLogins   = column[Int]("numLogins")
    def firstLogin  = column[Timestamp]("firstLogin")
    def lastLogin   = column[Timestamp]("lastLogin")

    def * = id ~ extId ~ name ~ email ~ numLogins ~ firstLogin ~ lastLogin <> (User, User.unapply _)
}

private object RouteTable extends Table[(Int, String, Double, Double, String, Double, Double, Double, Timestamp, Option[Int])]("Routes")
{
    def id              = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def routeData       = column[String]("routeData")
    def startLon	    = column[Double]("startLon")
    def startLat	    = column[Double]("startLat")
    def routeType       = column[String]("routeType")
    def distance        = column[Double]("distance")
    def ascent          = column[Double]("ascent")
    def duration        = column[Double]("duration")
    def timeAdded       = column[Timestamp]("timeAdded")
    def userId		    = column[Option[Int]]("userId")
    
    def * = id ~ routeData ~ startLon ~ startLat ~ routeType ~ distance ~ ascent ~ duration ~ timeAdded ~ userId
    def autoInc = (routeData ~ startLon ~ startLat ~ routeType ~ distance ~ ascent ~ duration ~ userId) returning id
}

private object RouteNameTable extends Table[(Int, String, String, Timestamp)]("RouteNames")
{
    def routeId     = column[Int]("routeId")
    def name   		= column[String]("name")
    def description	= column[String]("description")
    def timeAdded   = column[Timestamp]("timeAdded")
    
    def * = routeId ~ name ~ description ~ timeAdded
    def insCols = routeId ~ name ~ description
}

case class RouteSummary( routeId : Int, start : Coord, routeType : String, distance : Double, ascent : Double, duration : Double, userId : Option[Int] )

trait Persistence
{
    def getUser( extId : String ) : Option[User]
    def addUser( extId : String, email : String, name : String ) : User
    def addRoute( routeData : String, start : Coord, routeType : String, distance : Double, ascent : Double, duration : Double, userId : Option[Int] ) : Int
    def getRoute( routeId : Int ) : Option[RouteResult]
    def getRouteName( routeId : Int ) : Option[RouteName]
    def getRouteSummary( routeId : Int ) : Option[RouteSummary]
    def nameRoute( userId : Int, routeId : Int, name : String, description : String )
    def getUserRoutes( userId : Int ) : List[UserRoute]
    def getAllNamedRoutes() : List[UserRoute]
}

class DbPersistence( val db : Database ) extends Persistence
{
    implicit val formats = org.json4s.native.Serialization.formats(FullTypeHints( List(classOf[POIType]) ))
    private def timestampNow = new java.sql.Timestamp( (new java.util.Date()).getTime() )
    
    def getUser( extId : String ) : Option[User] =
    {
        db withTransaction
        {
            val thisUserQuery = Query(UserTable).filter( _.extId === extId )
            
            thisUserQuery.firstOption map
            { u =>
                
                // If this user exists, update numLogins and lastLogin
                thisUserQuery
                    .map( r => r.numLogins ~ r.lastLogin )
                    .update( (u.numLogins + 1, timestampNow) )
                
                u
            }
        }
    }
    
    def addUser( extId : String, email : String, name : String ) : User =
    {
        val insertCols = (UserTable.extId ~ UserTable.name ~ UserTable.email ~ UserTable.numLogins ~ UserTable.firstLogin ~ UserTable.lastLogin)
        
        val now = timestampNow
        
        db withTransaction
        {
            insertCols.insert( (extId, name, email, 0, now, now ) )
            
            Query(UserTable).filter( _.extId === extId ).firstOption.get
        }
    }
    
    def addRoute( routeData : String, start : Coord, routeType : String, distance : Double, ascent : Double, duration : Double, userId : Option[Int] ) : Int =
    {
        db withSession
        {
            RouteTable.autoInc.insert( (routeData, start.lon, start.lat, routeType, distance, ascent, duration, userId) )
        }
    }
    
    
    def getRoute( routeId : Int ) : Option[RouteResult] =
    {
        db withSession
        {
            val res = Query(RouteTable)
                .filter( _.id === routeId )
                .map( _.routeData )
                .firstOption
                
            res.map { s => sread[RouteResult](s) }
        }
    }
    
    def getRouteName( routeId : Int ) : Option[RouteName] =
    {
        db withSession
        {
            val resO = Query(RouteNameTable)
                .filter(_.routeId === routeId)
                .map( x => x.name ~ x.description ~ x.timeAdded )
                .firstOption
        
            resO.map { res => RouteName( res._1, res._2, res._3 ) }
        }
    }
    
    def getRouteSummary( routeId : Int ) : Option[RouteSummary] =
    {
        db withSession
        {
            val resO = Query(RouteTable)
                .filter(_.id === routeId )
                .map( x => x.id ~ x.startLon ~ x.startLat ~ x.routeType ~ x.distance ~ x.ascent ~ x.duration ~ x.userId )
                .firstOption
                
            resO.map( res => RouteSummary( res._1, Coord( res._2, res._3 ), res._4, res._5, res._6, res._7, res._8 ) )
        }
    }
    
    // You can name a route if you own it, or if it is currently un-owned (user None). In
    // the latter case, ownership will be assigned to the first namer.
    def nameRoute( userId : Int, routeId : Int, routeName : String, description : String )
    {
        db withTransaction
        {
            val routeUserQ = Query(RouteTable)
                .filter(_.id === routeId)
                .map(_.userId)
                
            val routeUserId = routeUserQ.first
            
            if ( routeUserId == None || routeUserId == Some(userId) )
            {
                routeUserQ.update( Some(userId) )
                RouteNameTable.insCols.insert( (routeId, routeName, description) )
            }
        }
    }
    
    def getUserRoutes( userId : Int ) : List[UserRoute] =
    {
        db withSession
        {
            val routes = for
            {
                rn  <- RouteNameTable
                r   <- RouteTable if rn.routeId === r.id && r.userId === userId
            } yield ( r.id, rn.name, rn.description, r.startLon, r.startLat, r.distance, r.ascent, r.timeAdded )
            
            routes.list.map
            { r =>
            	UserRoute(
            	    id = r._1,
            	    name = r._2,
            	    description = r._3,
            	    startCoord = Coord(r._4, r._5),
            	    distance = r._6,
            	    ascent = r._7,
            	    timeAdded = r._8 )
            }
        }
    }
    
    def getAllNamedRoutes() : List[UserRoute] =
    {
        db withSession
        {
            val routes = for
            {
                rn  <- RouteNameTable
                r   <- RouteTable if rn.routeId === r.id
            } yield ( r.id, rn.name, rn.description, r.startLon, r.startLat, r.distance, r.ascent, r.timeAdded )
            
            routes.list.map
            { r =>
            	UserRoute(
            	    id = r._1,
            	    name = r._2,
            	    description = r._3,
            	    startCoord = Coord(r._4, r._5),
            	    distance = r._6,
            	    ascent = r._7,
            	    timeAdded = r._8 )
            }
        }
    }
}
