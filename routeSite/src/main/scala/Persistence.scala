package org.seacourt.routeSite

import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.driver.H2Driver.simple._

case class User( id : Int, extId : String, name : String, email : String, numLogins : Int, firstLogin : java.sql.Timestamp, lastLogin : java.sql.Timestamp )

private object UserTable extends Table[User]("Users")
{
    def id          = column[Int]("id", O.PrimaryKey )
    def extId       = column[String]("extId")
    def name        = column[String]("name")
    def email       = column[String]("email")
    def numLogins   = column[Int]("numLogins")
    def firstLogin  = column[java.sql.Timestamp]("firstLogin")
    def lastLogin   = column[java.sql.Timestamp]("lastLogin")

    def * = id ~ extId ~ name ~ email ~ numLogins ~ firstLogin ~ lastLogin <> (User, User.unapply _)
}



class Persistence( val db : Database )
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
}
