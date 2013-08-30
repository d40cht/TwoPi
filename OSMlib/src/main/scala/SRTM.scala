package org.seacourt.osm

import java.io._

//import org.geotools.gce.geotiff.GeoTiffFormat
    

/*
    TFW file:
    Line 1: A: pixel size in the x-direction in map units/pixel
    Line 2: D: rotation about y-axis
    Line 3: B: rotation about x-axis
    Line 4: E: pixel size in the y-direction in map units, almost always negative[3]
    Line 5: C: x-coordinate of the center of the upper left pixel
    Line 6: F: y-coordinate of the center of the upper left pixel
*/

/*
case class TFW( xPixelSize : Double, xRot : Double, yRot : Double, yPixelSize : Double, xLeft : Double, yTop : Double )

object TFW
{
    def readFile( fname : File ) =
    {
        val s = io.Source.fromFile(fname)
        val fields = s.getLines.map( _.trim ).toArray
        val res = TFW(
            fields(0).toDouble,
            fields(1).toDouble,
            fields(2).toDouble,
            fields(3).toDouble,
            fields(4).toDouble,
            fields(5).toDouble )
        s.close
        
        res
    }
}

class SRTMTile( val file : File, val tfw : TFW )
{
    def apply( lon : Double, lat : Double ) =
    {
        //val outputFile = new java.io.File( fileName )
        //val format = new GeoTiffFormat()

        //assert( lon > 
    }
}

class SRTM( val directory : File )
{
    // Read the tiff using JAI?
    
    private val index : Array[(TFW, File)] =
    {
        val files = directory.listFiles.filter( _.toString.endsWith( ".tfw" ) )
        
        files.map
        { f =>
        
            val tfw = TFW.readFile(f)
            val data = new File( f.toString.dropRight(4) + ".tif" )
            assert( data.exists )
            
            (tfw, data)
        }.toArray
    }
    
    // Use an LRU cache to keep tiffs in memory. When adding heights to nodes,
    // sort by lon then lat to keep the cached files hot.
    def height( c : Coord ) : Double =
    {
        0.0
    } 
}
*/


