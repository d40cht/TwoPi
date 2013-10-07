package org.seacourt.routeSite

import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.driver.H2Driver.simple._

import java.sql.{Timestamp}

case class User( id : Int, extId : String, name : String, email : String, numLogins : Int, firstLogin : Timestamp, lastLogin : Timestamp )
case class UserRoute( id : Int, name : String, distance : Double, ascent : Double, timeAdded : Timestamp )

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

private object RouteTable extends Table[(Int, String, Double, Double, Timestamp)]("Routes")
{
    def id          = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def routeData   = column[String]("routeData")
    def distance    = column[Double]("distance")
    def ascent      = column[Double]("ascent")
    def timeAdded   = column[Timestamp]("timeAdded")
    
    def * = id ~ routeData ~ distance ~ ascent ~ timeAdded
    def autoInc = (routeData ~ distance ~ ascent) returning id
}

private object UserRouteTable extends Table[(Int, Int, String, Timestamp)]("UserRoutes")
{
    def routeId     = column[Int]("routeId")
    def userId      = column[Int]("userId")
    def routeName   = column[String]("routeName")
    def timeAdded   = column[Timestamp]("timeAdded")
    
    def * = routeId ~ userId ~ routeName ~ timeAdded
    def insCols = routeId ~ userId ~ routeName
}

trait Persistence
{
    def getUser( extId : String ) : Option[User]
    def addUser( extId : String, email : String, name : String ) : User
    def addRoute( routeJSON : String, distance : Double, ascent : Double ) : Int
    def getRoute( routeId : Int ) : Option[String]
    def saveRouteToUser( userId : Int, routeId : Int, routeName : String )
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
    
    def addRoute( routeData : String, distance : Double, ascent : Double ) : Int =
    {
        db withSession
        {
            RouteTable.autoInc.insert( (routeData, distance, ascent) )
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
    
    def saveRouteToUser( userId : Int, routeId : Int, routeName : String )
    {
        db withSession
        {
            UserRouteTable.insCols.insert( (routeId, userId, routeName) )
        }
    }
    
    def getUserRoutes( userId : Int ) : List[UserRoute] =
    {
        db withSession
        {
            val routes = for
            {
                ur  <- UserRouteTable
                r   <- RouteTable if ur.routeId === r.id
            } yield ( ur.routeId, ur.routeName, r.distance, r.ascent, r.timeAdded )
            
            routes.list.map( r => UserRoute( r._1, r._2, r._3, r._4, r._5 ) )
        }
    }
}
