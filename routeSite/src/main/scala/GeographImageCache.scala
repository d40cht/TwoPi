package org.seacourt.routeSite

import org.scalatra._
import scalate.ScalateSupport

import org.seacourt.osm.Logging

class GeographImageServlet extends ScalatraServlet
{
    private val geoGraphCache = new java.io.File( "geographCache" )
    if ( !geoGraphCache.exists() ) geoGraphCache.mkdirs()
    
    private def imgUrl( index : Long, hash : String ) : String =
    {
        val yz = index / 1000000
        val ab = (index % 1000000) / 10000
        val cd = (index % 10000) / 100
        
        val fullPath = if ( yz == 0 )
        {
            "/photos/%02d/%02d/%06d_%s".format( ab, cd, index, hash )
        }
        else
        {
            "/geophotos/%02d/%02d/%02d/%06d_%s".format( yz, ab, cd, index, hash )
        }
        
        fullPath
    }
    
    private def cacheToFile( url : String, fileName : java.io.File ) =
    {
        import scalaj.http.{Http, HttpOptions}
        val res = Http(url)
            .option(HttpOptions.connTimeout(1500))
            .option(HttpOptions.readTimeout(1500))
        { inputStream =>
        
            val os = new java.io.FileOutputStream( fileName )
            try
            {   
                org.apache.commons.io.IOUtils.copy( inputStream, os )
            }
            finally
            {
                os.close
            }
        }
    }
    
    get("/thumb/:id/:hash")
    {
        contentType = "image/jpeg"
        
        val id = params("id").toLong
        val hash = params("hash")
        val fullPath = imgUrl( id, hash )
        
        val url = "http://s%d.geograph.org.uk%s_213x160.jpg".format( id % 4, fullPath )
        val fname = new java.io.File( geoGraphCache, "%d_thumb.jpg".format( id ) )
        cacheToFile( url, fname )
        
        fname
    }
    
    get("/full/:id/:hash")
    {
        contentType = "image/jpeg"
        
        val id = params("id").toLong
        val hash = params("hash")
        val fullPath = imgUrl( id, hash )
        
        val url = "http://s0.geograph.org.uk%s.jpg".format( fullPath )
        val fname = new java.io.File( geoGraphCache, "%d.jpg".format( id ) )
        cacheToFile( url, fname )
        
        fname
    }
}

 
