package org.seacourt.routeSite

import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.driver.H2Driver.simple._

import java.sql.{Timestamp}

import org.seacourt.osm.Coord

case class User( id : Int, extId : String, name : String, email : String, numLogins : Int, firstLogin : Timestamp, lastLogin : Timestamp )
case class UserRoute( id : Int, name : String, description : String, startCoord : Coord, distance : Double, ascent : Double, timeAdded : Timestamp )

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

private object RouteTable extends Table[(Int, String, Double, Double, Double, Double, Timestamp, Option[Int])]("Routes")
{
    def id          = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def routeData   = column[String]("routeData")
    def startLon	= column[Double]("startLon")
    def startLat	= column[Double]("startLat")
    def distance    = column[Double]("distance")
    def ascent      = column[Double]("ascent")
    def timeAdded   = column[Timestamp]("timeAdded")
    def userId		= column[Option[Int]]("userId")
    
    def * = id ~ routeData ~ startLon ~ startLat ~distance ~ ascent ~ timeAdded ~ userId
    def autoInc = (routeData ~ startLon ~ startLat ~ distance ~ ascent ~ userId) returning id
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

trait Persistence
{
    def getUser( extId : String ) : Option[User]
    def addUser( extId : String, email : String, name : String ) : User
    def addRoute( routeData : String, start : Coord, distance : Double, ascent : Double, userId : Option[Int] ) : Int
    def getRoute( routeId : Int ) : Option[String]
    def nameRoute( userId : Int, routeId : Int, name : String, description : String )
    def getUserRoutes( userId : Int ) : List[UserRoute]
}

class DbPersistence( val db : Database ) extends Persistence
{
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
    
    def addRoute( routeData : String, start : Coord, distance : Double, ascent : Double, userId : Option[Int] ) : Int =
    {
        db withSession
        {
            RouteTable.autoInc.insert( (routeData, start.lon, start.lat, distance, ascent, userId) )
        }
    }
    
    def getRoute( routeId : Int ) : Option[String] =
    {
        db withSession
        {
            val res = Query(RouteTable)
                .filter( _.id === routeId )
                .map( _.routeData )
                .firstOption
                
            res
        }
    }
    
    def nameRoute( userId : Int, routeId : Int, routeName : String, description : String )
    {
        db withSession
        {
            RouteNameTable.insCols.insert( (routeId, routeName, description) )
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
}
