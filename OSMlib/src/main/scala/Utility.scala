package org.seacourt.osm

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{ Input, Output }
import com.twitter.chill._
import org.objenesis.strategy.StdInstantiatorStrategy

import java.io._
import java.util.zip._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object Utility extends Logging
{
    def kryoSave[T]( obj : T, outputFile : File, kryo : Kryo = new Kryo() ) =
    {
        kryo.setInstantiatorStrategy(new StdInstantiatorStrategy())
        val output = new Output( new GZIPOutputStream( new FileOutputStream( outputFile ) ) )
        kryo.writeObject(output, obj)
        output.close
    }
    
    def kryoLoad[T]( inputFile : File, kryo : Kryo = new Kryo() )( implicit tag : ClassTag[T] ) : T =
    {
        kryo.setInstantiatorStrategy(new StdInstantiatorStrategy())
        val input = new Input( new GZIPInputStream( new java.io.FileInputStream( inputFile ) ) )
        val obj = kryo.readObject( input, tag.runtimeClass ).asInstanceOf[T]
        input.close
        obj
    }
    
    def kryoCache[T]( cacheFileName : File, generateFn : => T, kryo : Kryo = new Kryo() )( implicit tag : ClassTag[T] ) : T =
    {
        if ( cacheFileName.exists )
        {
            log.info( "Loading data from cache: " + cacheFileName )
            val res = kryoLoad[T]( cacheFileName, kryo )(tag)
            log.info( "...complete" )
            res
        }
        else
        {
            log.info( "Generating data for: " + cacheFileName )
            val res = generateFn
            log.info( "Saving data for: " + cacheFileName )
            kryoSave( res, cacheFileName, kryo )
            log.info( "...complete" )
            res
        }
    }
}

