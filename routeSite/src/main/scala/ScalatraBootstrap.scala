

import org.scalatra._
import javax.servlet.ServletContext

// Support for slick and c3p0 connection pooling
import com.mchange.v2.c3p0.ComboPooledDataSource
import org.slf4j.{Logger, LoggerFactory}
import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.driver.H2Driver.simple._
import scala.slick.jdbc.{StaticQuery}
import scala.slick.jdbc.meta.{MTable}

package org.seacourt.routeSite
{

    trait DatabaseEvolutionManager
    {
        def logger : Logger
        
        object DbEvolutions extends Table[(Int, String, Boolean)]("DbEvolutions")
        {
            def version     = column[Int]("version", O.PrimaryKey )
            def scriptHash  = column[String]("scriptHash")
            def cleanApply  = column[Boolean]("cleanApply")
            def * = version ~ scriptHash ~ cleanApply
        }
        
        val db : Database
        
        private def applyEvolutionScript( script : String, version : Int, scriptHash : String )
        { 
            // DDL (which will form the main part of evolution scripts) is not transactional
            // on H2. So there's no point running these scripts in a transaction
            DbEvolutions.insert( (version, scriptHash, false) )
            script.split(";").foreach
            { cmd =>
            
                logger.info( "Applying: " + cmd )
                StaticQuery.updateNA(cmd).execute
            }
            // If we get here, mark the evolution as cleanly applied
            Query(DbEvolutions)
                .filter( _.version === version )
                .map( _.cleanApply )
                .update(true)
            
        }
        
        def evolveDb( evolutions : Seq[(Int, String)] )
        {
            db withSession
            {
                if ( MTable.getTables.list.isEmpty )
                {
                    DbEvolutions.ddl.create
                }
            
                val appliedEvolutions = Query(DbEvolutions)
                    .list
                    .sortBy( _._1 )
                    
                 
                // Check that we have a dense series of version numbers for applied evolutions
                appliedEvolutions.zipWithIndex.foreach
                { case ((v, hash, clean), i) =>
                    assert( v == i, "Existing evolutions are not well-formed: " + appliedEvolutions.map(_._1).mkString(",") )
                    assert( clean, "Evolution was not cleanly applied: " + i )
                }
                
                assert( appliedEvolutions.isEmpty || evolutions.last._1 >= appliedEvolutions.last._1, "This application package is at an older version than the database" )
               
                
                val evoMap = appliedEvolutions.map( x => (x._1, x._2) ).toMap
                for ( (version, evolutionScript) <- evolutions )
                {
                    val computedHash = org.seacourt.osm.Utility.shaHash( evolutionScript )
                    evoMap.get(version) match
                    {
                        case Some(hash) =>
                        {
                            assert( hash == computedHash, "Hash of evolution %d in db does not match the evo script: %s, %s".format(version, hash, computedHash) )
                            logger.info( "Skipping already applied evolution (%d)".format( version ) )
                        }
                        case None =>
                        {
                            // Apply this evolution script
                            logger.info( "Applying evolution: %d".format(version) )
                            applyEvolutionScript( evolutionScript, version, computedHash )
                        }
                    }
                }   
            }    
        }
    }

}

class ScalatraBootstrap extends LifeCycle with org.seacourt.routeSite.DatabaseEvolutionManager
{
    val logger = LoggerFactory.getLogger(getClass)
    val cpds = new ComboPooledDataSource
    logger.info("Created c3p0 connection pool")
    
    
    val db = Database.forDataSource(cpds)
    val persistence = new org.seacourt.routeSite.DbPersistence(db)
    
    private def evolveFromJar() =
    {
        val resourcePath = "dbevolutions/"
        val classLoader = getClass().getClassLoader()
        
        // Yes, I know this is horrible. But there isn't a way to list resource paths
        // in a jar, so this'll do for now.
        val evolutionsFromJar = Stream.from(0)
            .map( version => (version, classLoader.getResourceAsStream( resourcePath + version.toString + ".sql" )) )
            .takeWhile( _._2 != null )
            .map
            { case (version, istream) =>
            
                val writer = new java.io.StringWriter()
                org.apache.commons.io.IOUtils.copy( istream, writer, "utf-8")
                (version, writer.toString)
            }
            .toSeq
            
        logger.info( "Most recent evolution in JAR: " + evolutionsFromJar.last.toString )
            
        evolveDb( evolutionsFromJar )
    }
    
    evolveFromJar()
    
    override def init(context: ServletContext)
    {
        context.mount(new org.seacourt.routeSite.GeographImageServlet, "/geograph/*")
        context.mount(new org.seacourt.routeSite.RouteSiteServlet(persistence), "/*")
    }
    
    private def closeDbConnection()
    {
        logger.info("Closing c3p0 connection pool")
        cpds.close
    }

    override def destroy(context: ServletContext)
    {
        super.destroy(context)
        closeDbConnection
    }
}

