package org.seacourt.osm


import java.io.{File, BufferedInputStream, FileInputStream}

import crosby.binary.osmosis.OsmosisReader

import org.openstreetmap.osmosis.core.task.v0_6.Sink
import org.openstreetmap.osmosis.core.container.v0_6.EntityContainer
import org.openstreetmap.osmosis.core.domain.v0_6

import scala.collection.{mutable, immutable}

case class Tag( val key : String, val value : String )
case class Coord( val lon : Double, val lat : Double )
case class Node( val id : Long, val coord : Coord, val tags : Array[Tag] )
case class Way( val id : Long, val nodes : Array[Node], val tags : Array[Tag] )

class CrunchSink extends Sink
{
    import scala.collection.JavaConversions._
    
    def inUk( c : Coord ) = c.lon > -9.23 && c.lon < 2.69 && c.lat > 49.84 && c.lat < 60.85
        
    var ukNodes = 0
    var ukWays = 0
    
    def initialize(metaData : java.util.Map[String, Object])
    {
        println( "initialize" )
    }
    
    val nodesById = mutable.Map[Long, Node]()
    val ways = mutable.ArrayBuffer[Way]()
    
    def process(entityContainer : EntityContainer)
    {
        val entity = entityContainer.getEntity()
        
        entity match
        {
            case n : v0_6.Node =>
            {
                val c = new Coord( n.getLongitude(), n.getLatitude() )
                
                if ( inUk(c) )
                {
                    ukNodes += 1
                    if ( (ukNodes % 100000) == 0 ) println( "Nodes: " + ukNodes )
                    
                    nodesById.put( n.getId(), Node( n.getId(), c, n.getTags().map { t => Tag( t.getKey(), t.getValue() ) }.toArray ))
                }
            }
            
            case w : v0_6.Way =>
            {
                val nodeIds : Array[Long] = w.getWayNodes().map( _.getNodeId() ).toArray
                
                if ( nodeIds.forall( nid => nodesById contains nid ) )
                {
                    val nodes = nodeIds.map( nid => nodesById(nid) )
                    ukWays += 1
                    if ( (ukWays % 10000) == 0 ) println( "Ways: " + ukWays )
                    val way = Way( w.getId(), nodes, w.getTags().map { t => Tag( t.getKey(), t.getValue() ) }.toArray )
                    ways.append(way)
                }
            }
            
            case _ =>
        }
        // entity.getId() : Lo
        // .getVersion() : Int
        // .getTimestamp() : Date
        // getUser() : OsmUser (.getId() : Int, .getName() : String)
        // Collection<Tag> getTags() (.getKey(), .getValue())
        // Map<String, Object> getMetaTags()
        
        // Way extends Entity
        // List<WayNode> getWayNodes() (.getNodeId() : Long)
        
        // class Node extends Entity
        // getLatitude, getLongitude
    }
    
    def complete()
    {
        println( "complete" )
    }
    
    def release()
    {
        println( "release" )
    }
}

class OSMCrunch( val dataFileName : File )
{
    def run()
    {
        val reader = new OsmosisReader( new BufferedInputStream( new FileInputStream( dataFileName ) ) )
        val cs = new CrunchSink()
        reader.setSink( cs )
        
        reader.run()
    }
}

object OSMCrunch extends App
{
    override def main( args : Array[String] )
    {
        val osmc = new OSMCrunch( new File("/backup/Data/OSM/europe-latest.osm.pbf") )
        osmc.run()
    }
}
