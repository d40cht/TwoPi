package org.seacourt.osm

//import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{ Input, Output }
import java.io._
import java.util.zip._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object Utility extends Logging
{
    def kryoSave[T]( obj : T, outputFile : File ) =
    {
        import com.twitter.chill._
        val kryo = KryoSerializer.registered.newKryo

        val output = new Output( new GZIPOutputStream( new FileOutputStream( outputFile ) ) )
        kryo.writeObject(output, obj)
        output.close
    }
    
    def kryoLoad[T]( inputFile : File )( implicit tag : ClassTag[T] ) : T =
    {
        import com.twitter.chill._
        val kryo = KryoSerializer.registered.newKryo

        val input = new Input( new GZIPInputStream( new FileInputStream( inputFile ) ) )
        val obj = kryo.readObject( input, tag.runtimeClass ).asInstanceOf[T]
        input.close
        obj
    }
    
    def kryoCache[T]( cacheFileName : File, generateFn : => T )( implicit tag : ClassTag[T] ) : T =
    {
        import com.twitter.chill._
        if ( cacheFileName.exists )
        {
            log.info( "Loading data from cache: " + cacheFileName )
            val res = kryoLoad[T]( cacheFileName )(tag)
            log.info( "...complete" )
            res
        }
        else
        {
            log.info( "Generating data for: " + cacheFileName )
            val res = generateFn
            log.info( "Saving data for: " + cacheFileName )
            kryoSave( res, cacheFileName )
            log.info( "...complete" )
            res
        }
    }
    
    def shaHash(s: String) : String =
    {
        import java.security.MessageDigest
        MessageDigest.getInstance("SHA").digest(s.getBytes).map( x => "%02x".format(x) ).mkString
    }
}

