package org.seacourt.osm.test

import org.scalatest.{FunSuite, FlatSpec}
import org.slf4j.{Logger, LoggerFactory}
import scala.slick.session.Database
import org.scalatra._
import Database.threadLocalSession
import scala.slick.driver.H2Driver.simple._
import scala.slick.jdbc.{StaticQuery}

import org.seacourt.routeSite._

import scala.collection.{mutable, immutable}

case class DBEvolverTest( val db : Database ) extends DatabaseEvolutionManager
{
    val logger = LoggerFactory.getLogger(getClass)
}

class DatabaseSimpleTest extends FlatSpec
{
    object DbFoo extends Table[(Int, String)]("DbFoo")
    {
        def version     = column[Int]("version", O.PrimaryKey )
        def scriptHash  = column[String]("scriptHash")
        def * = version ~ scriptHash
    }
    
    object AutoIncTable extends Table[(Int, String)]("AutoInc")
    {
        def id          = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def text        = column[String]("text")
        
        def * = id ~ text
        def autoInc = text returning id
    }
    
    val db = Database.forURL("jdbc:h2:mem:testSimple;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
 
    "A database" should "persist data that has been inserted into it" in
    {
        db withSession
        {
            DbFoo.ddl.create
            AutoIncTable.ddl.create
            assert( Query(DbFoo).list.length === 0 )
            
            DbFoo.insert( (3, "Foo") )
            assert( Query(DbFoo).list.length === 1 )
            
            DbFoo.insert( (2, "Bar") )
            assert( Query(DbFoo).list.length === 2 )
            
            DbFoo.insert( (1, "Baz") )
            assert( Query(DbFoo).list.length === 3 )
        }
    }
    
    "A database" should "remember values between sessions" in
    {
        db withSession
        {
            assert( Query(DbFoo).list.length === 3 )
            
            assert( Query(DbFoo).sortBy( _.scriptHash ).map( _.scriptHash ).list === List( "Bar", "Baz", "Foo" ) )
            assert( Query(DbFoo).sortBy( _.version ).map( _.version ).list === List( 1, 2, 3 ) )
        }
    }
    
    "Slick" should "allow autoincrement columns to be returned after inserts" in
    {
        db withSession
        {
            val res1 = AutoIncTable.autoInc.insert("a")
            assert( res1 === 1 )
            val res2 = AutoIncTable.autoInc.insert("b")
            assert( res2 === 2 )
            val res3 = AutoIncTable.autoInc.insert("c")
            assert( res3 === 3 )
            
            val allData = Query(AutoIncTable).list
            
            assert( allData === List(
                (1, "a"),
                (2, "b"),
                (3, "c") ) )
        }
    }
}



class DatabaseEvolutionsTest extends FlatSpec
{    
    val db = Database.forURL("jdbc:h2:mem:testEvo;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
    
    "Database evolutions" should "only create the evolutions table once" in
    {
        // Running this twice should be fine (shouldn't try to create the evolution table again)
        {
            val evolver = DBEvolverTest( db )
            evolver.evolveDb( Seq() )
        }
        
        {
            val evolver = DBEvolverTest( db )
            evolver.evolveDb( Seq() )
        }
    }
    
    val evolver = DBEvolverTest( db )
    "Database evolutions" should "allow changes" in
    {
        evolver.evolveDb( Seq( (0, "CREATE TABLE foo( id INTEGER PRIMARY KEY AUTO_INCREMENT )") ) )
    }
        
    "Database evolutions" should "stop on malformed SQL" in
    {
        // Expect an error on a malformed statement
        intercept[org.h2.jdbc.JdbcSQLException]
        {
            evolver.evolveDb( Seq(
                (0, "CREATE TABLE foo( id INTEGER PRIMARY KEY AUTO_INCREMENT )"),
                (1, "FOO BAR") ) )
        }
    }
    
    "Database evolutions" should "disallow further mutation until the db is cleaned up" in
    {   
        // Expect an assertion to fail because the evolution was not cleanly applied
        intercept[java.lang.AssertionError]
        {
            evolver.evolveDb( Seq( (0, "CREATE TABLE foo( id INTEGER PRIMARY KEY AUTO_INCREMENT )") ) )
        }
        
        db withSession
        {
            StaticQuery.updateNA( "DELETE FROM \"DbEvolutions\" WHERE \"version\"=1" ).execute
        }
        
        // Add another table
        evolver.evolveDb( Seq(
            (0, "CREATE TABLE foo( id INTEGER PRIMARY KEY AUTO_INCREMENT )"),
            (1, "CREATE TABLE foo2( id INTEGER, val VARCHAR )")
        ) )
    }
    
    
    "Database evolutions" should "do nothing when the evolutions do not differ from the current DB state" in
    {    
        // Expect no error for a repeat of the previous application
        evolver.evolveDb( Seq(
            (0, "CREATE TABLE foo( id INTEGER PRIMARY KEY AUTO_INCREMENT )"),
            (1, "CREATE TABLE foo2( id INTEGER, val VARCHAR )")
        ) )
        
    }
    
    "Database evolutions" should "throw errors if the evolutions do not match those already applied" in
    {   
        // Expect an error if either of the previous two are different
        intercept[java.lang.AssertionError]
        {
            evolver.evolveDb( Seq(
                (0, "CREATE TABLE foo( ido INTEGER PRIMARY KEY AUTO_INCREMENT )"),
                (1, "CREATE TABLE foo2( id INTEGER, val VARCHAR )")
            ) )
        }
        
        
        intercept[java.lang.AssertionError]
        {
            evolver.evolveDb( Seq(
                (0, "CREATE TABLE foo( id INTEGER PRIMARY KEY AUTO_INCREMENT )"),
                (1, "CREATE TABLE foo2( ido INTEGER, val VARCHAR )")
            ) )
        }
    }
    
    "Database evolutions" should "keep and correctly transform existing data" in
    {  
        // Add some data
        db withSession
        {
            StaticQuery.updateNA( "INSERT INTO foo2 VALUES( 1, 'foo' )" ).execute
            StaticQuery.updateNA( "INSERT INTO foo2 VALUES( 2, 'bar' )" ).execute
            
            
            val data = StaticQuery.queryNA[(Int, String)]("SELECT * FROM foo2").list
            assert( data === List( (1, "foo"), (2, "bar") ) )
        }
        
        evolver.evolveDb( Seq(
                    (0, "CREATE TABLE foo( id INTEGER PRIMARY KEY AUTO_INCREMENT )"),
                    (1, "CREATE TABLE foo2( id INTEGER, val VARCHAR )"),
                    (2,
                    """
                    |  ALTER TABLE foo2 ADD COLUMN idDoubled INT;
                    |  UPDATE foo2 SET idDoubled = id*2
                    """.stripMargin('|') )
            ) )
            
        db withSession
        {
            StaticQuery.updateNA( "INSERT INTO foo2 VALUES( 3, 'qux', 9 )" ).execute
            
            
            val data = StaticQuery.queryNA[(Int, String, Int)]("SELECT * FROM foo2").list
            assert( data === List( (1, "foo", 2), (2, "bar", 4), (3, "qux", 9) ) )
        }
    }
    
    "Database evolutions" should "abort if you have a database that is a newer schema than the code being run" in
    {  

        intercept[java.lang.AssertionError]
        {
            db withSession
            {
                evolver.evolveDb( Seq(
                    (0, "CREATE TABLE foo( id INTEGER PRIMARY KEY AUTO_INCREMENT )")
                ) )
                
            }
        }
    }
}
