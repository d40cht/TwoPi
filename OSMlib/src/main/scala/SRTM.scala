package org.seacourt.osm

import java.io._
import java.util.zip._

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{ Input, Output }
import com.twitter.chill._


case class SRTMInMemoryTiles( files : Seq[File] )
{
    private val tiles = files.map( f => SRTMInMemoryTile(f) ).toList
    
    def elevation( lon : Double, lat : Double ) : Option[Double] =
    {
        for ( t <- tiles )
        {
            val res = t.elevation( lon, lat )
            
            res.foreach( el => return Some(el) )
        }
        
        //println( "Point outside range: " + lon + ", " + lat )
        
        return None
    }
}


// Mapping tested manually using: http://www.daftlogic.com/sandbox-google-maps-find-altitude.htm
case class SRTMInMemoryTile( val lonMin : Double, val latMin : Double, val nRows : Int, val nCols : Int, val cellSize : Double, val allData : Array[Short] )
{
    def this() = this(0.0, 0.0, 0, 0, 0, Array() )
    
    assert( allData.size == nRows * nCols )
    
    val lonMax = lonMin + ((nCols-1).toDouble * cellSize)
    val latMax = latMin + ((nRows-1).toDouble * cellSize)
    
    println( "Tile bounds : %f, %f, %f, %f".format( lonMin, lonMax, latMin, latMax ) )
    println( "Data points: " + allData.size.toString )
    
    def inRange( lon : Double, lat : Double ) : Boolean = lon >= lonMin && lon <= lonMax && lat >= latMin && lat <= latMax
    
    private def get( x : Int, y : Int ) : Option[Double] =
    {
        val index = x + (((nRows-1)-y)*nCols)
        
        if ( index >= allData.size ) None
        else
        {
            val height = allData(index)
            if ( height == -9999 ) None
            else Some(height.toDouble)
        }
    }
    
    private def bilinearInterpolation( Q11 : Double, Q21 : Double, Q12 : Double, Q22 : Double, xFrac : Double, yFrac : Double ) =
    {
        assert( xFrac >= 0.0 && xFrac <= 1.0 )
        assert( yFrac >= 0.0 && yFrac <= 1.0 )
        
        Q11 * (1.0 - xFrac) * (1.0 - yFrac) +
        Q21 * (1.0 - xFrac) * yFrac +
        Q12 * xFrac * (1.0 - yFrac) +
        Q22 * xFrac * yFrac
    }
    
    
    // Interpolated elevation, bilinear from: http://en.wikipedia.org/wiki/Bilinear_interpolation
    def elevation( lon : Double, lat : Double ) : Option[Double] =
    {
        if ( inRange( lon, lat ) )
        {
            val x1 = ((lon - lonMin) / cellSize).toInt
            val y1 = ((lat - latMin) / cellSize).toInt
            
            val el11O = get( x1, y1 )
            val el12O = get( x1, y1 + 1 )
            val el21O = get( x1 + 1, y1 )
            val el22O = get( x1 + 1, y1 + 1 )
            
            (el11O, el12O, el21O, el22O) match
            {
                case (Some(q11), Some(q12), Some(q21), Some(q22)) =>
                {
                    val xFrac = (lon - lonMin) - (x1*cellSize)
                    val yFrac = (lat - latMin) - (y1*cellSize)
                    
                    Some( bilinearInterpolation( q11, q21, q12, q22, xFrac, yFrac ) )
                }
                
                case _ => None
            }
        }
        else None
    }
}

/*
    xll and yll and western and southern corner coordinates, x increments eastwards, y increments northwards
    
    Row 0: ncols
    Row 1: nrows
    Row 2: xllcorner
    Row 3: yllcorner
    Row 4: cellsize
    Row 5: NODATA_value
*/

object SRTMInMemoryTile extends org.seacourt.osm.Logging
{
    def apply( f : java.io.File ) =
    {
        val cacheFile = new java.io.File( f + ".cache" )
        if ( !cacheFile.exists )
        { 
            log.info( "Loading from SRTM ascii files" )
            val lines = io.Source.fromFile( f ).getLines
            val prefixLines = lines.take(6).toList
            println( prefixLines )
            val prefix = prefixLines.map( _.split(" ").last.trim ).toSeq
            
            val ncols = prefix(0).toInt
            val nrows = prefix(1).toInt
            val cellSize = prefix(4).toDouble
            val lonMin = prefix(2).toDouble
            val latMin = prefix(3).toDouble
            
            val allData = lines.flatMap( _.split(" ").map( _.trim.toShort ) )
            val tile = new SRTMInMemoryTile( lonMin, latMin, nrows, ncols, cellSize, allData.toArray )
            
            log.info( "Caching tile to: " + cacheFile )
            val kryo = new Kryo()
            val output = new Output( new GZIPOutputStream( new FileOutputStream( cacheFile ) ) )
            kryo.writeObject(output, tile)
            output.close
            
            tile
        }
        else
        {
            log.info( "Loading from cache: " + cacheFile )
            val kryo = new Kryo()
            val input = new Input( new GZIPInputStream( new java.io.FileInputStream( cacheFile ) ) )
            
            kryo.readObject( input, classOf[SRTMInMemoryTile] )
        }
    }
}
  
    

