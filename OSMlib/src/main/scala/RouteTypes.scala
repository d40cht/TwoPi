package org.seacourt.osm.route

import org.seacourt.osm._
import org.seacourt.osm.poi.POITypes._

import scala.collection.{mutable, immutable}


// 1.0 represents as good as possible, 0.0 represents as bad as possible
case class Score( val value : Double )
{
    assert( value >= 0.0 && value <= 1.0, "Value outside range: " + value )
    
    def *( other : Score ) = Score( value * other.value )
    def isZero = value == 0.0
}

case class Speed( val kph : Double )
{
    def timeToCover( distMeters : Double ) =
    {
        assert( kph > 0.0, "Invalid speed: " + kph)
        val mps = kph / (3600.0 / 1000.0)
        distMeters / mps
    }
}

trait RouteType
{
    def score( re : RouteEdge ) : Score
    def speed( re : RouteEdge ) : Speed
    def respectOneWay : Boolean
    
    def scenicScore( re : RouteEdge ) : Score =
    {
    	if ( !re.scenicPoints.isEmpty )
	    {
	        // 1 or 2 are rubbish, 8 or 9 are exceptional, but 4 can be quite nice
	        val maxScore = re.scenicPoints.map( _.score ).max
	        
	        if ( maxScore < 1 )         Score(0.3)
	        else if ( maxScore < 2 )    Score(0.4)
	        else if ( maxScore < 4 )    Score(0.6)
	        else if ( maxScore < 5 )    Score(0.7)
	        else if ( maxScore < 6 )    Score(0.8)
	        else if ( maxScore < 7 )    Score(0.9)
	        else                        Score(1.0)
	    }
	    else
	    {
	        Score(0.5)
	    }
    }
    
    def validDests( rn : RouteNode ) : Seq[EdgeDest] =
    {
        rn.destinations.filter( de => !score(de.edge).isZero )
    }
}

class DrivingRoute extends RouteType
{
    override def score( re : RouteEdge ) : Score =
	{
    	val tagMap = re.wayTags
    	
        val highwayAnnotation : Option[String] = tagMap.get("highway")
        val junctionAnnotation : Option[String] = tagMap.get("junction")
        val bridgeAnnotation : Option[String] = tagMap.get("bridge")
        val nameAnnotation : Option[String] = tagMap.get("name")
        val refAnnotation : Option[String] = tagMap.get("ref")
        val footAnnotation : Option[String] = tagMap.get("foot")
    
        // Other important things:
        // ford: yes - In the case of Duxford Ford, this is not fordable.
        val costMultiplierOption = highwayAnnotation match
        {
            case Some( valueString ) =>
            {
                if ( valueString.startsWith( "motorway" ) )                				Some( Score(1.0) )
                else if ( valueString.startsWith( "trunk" ) )                			Some( Score(1.0) )
                else if ( valueString.startsWith( "primary" ) )         				Some( Score(0.9) )
                else if ( valueString.startsWith( "road" ) )            				Some( Score(0.6) )
                else if ( valueString.startsWith( "secondary" ) )       				Some( Score(0.8) )
                else if ( valueString.startsWith( "residential" ) )     				Some( Score(0.4) )
                else if ( valueString.startsWith( "tertiary" ) )        				Some( Score(0.5) )
                else None
            }
            case None => None
        }
        
        val costMultiplier = costMultiplierOption.getOrElse( Score(0.0) )
    
	    val sc = scenicScore( re )
	    
	    costMultiplier * sc * Score(re.landCoverScore)
	}
    
    override def speed( re : RouteEdge ) =
    {
    	val tagMap = re.wayTags
    	
        val highwayAnnotation : Option[String] = tagMap.get("highway")
        val junctionAnnotation : Option[String] = tagMap.get("junction")
        val bridgeAnnotation : Option[String] = tagMap.get("bridge")
        val nameAnnotation : Option[String] = tagMap.get("name")
        val refAnnotation : Option[String] = tagMap.get("ref")
        val footAnnotation : Option[String] = tagMap.get("foot")
    
        highwayAnnotation match
        {
            case Some( valueString ) =>
            {
                if ( valueString.startsWith( "motorway" ) )             Speed(115.0)
                else if ( valueString.startsWith( "trunk" ) )           Speed(115.0)
                else if ( valueString.startsWith( "primary" ) )         Speed(100.0)
                else if ( valueString.startsWith( "road" ) )            Speed(30.0)
                else if ( valueString.startsWith( "unclassified" ) )    Speed(30.0)
                else if ( valueString.startsWith( "secondary" ) )       Speed(80.0)
                else if ( valueString.startsWith( "residential" ) )		Speed(30.0)
                else if ( valueString.startsWith( "tertiary" ) )       	Speed(40.0)
                else Speed(0.0)
            }
            case None => Speed( kph = 0.0)
        }
    }
    override def respectOneWay = true
}

class WalkingRoute extends RouteType
{
    override def score( re : RouteEdge ) : Score =
	{
    	val tagMap = re.wayTags
    	
        val highwayAnnotation : Option[String] = tagMap.get("highway")
        val junctionAnnotation : Option[String] = tagMap.get("junction")
        val bridgeAnnotation : Option[String] = tagMap.get("bridge")
        val nameAnnotation : Option[String] = tagMap.get("name")
        val refAnnotation : Option[String] = tagMap.get("ref")
        val footAnnotation : Option[String] = tagMap.get("foot")
    
        // Other important things:
        // ford: yes - In the case of Duxford Ford, this is not fordable.
        val costMultiplierOption = highwayAnnotation match
        {
            case Some( valueString ) =>
            {
                if ( refAnnotation.isDefined && refAnnotation.get.matches("A[0-9]+") ) 	Some( Score(0.05) )
                else if ( valueString.startsWith( "trunk" ) )                			Some( Score(0.05) )
                else if ( valueString.startsWith( "primary" ) )         				Some( Score(0.1) )
                else if ( valueString.startsWith( "road" ) )            				Some( Score(0.4) )
                else if ( valueString.startsWith( "secondary" ) )       				Some( Score(0.4) )
                // Because they're really boring
                else if ( valueString.startsWith( "residential" ) )     				Some( Score(0.4) )
                else if ( valueString.startsWith( "cycleway" ) )        				Some( Score(0.5) )
                else if ( valueString.startsWith( "tertiary" ) )        				Some( Score(0.6) )
                else if ( valueString.startsWith( "unclassified" ) )    				Some( Score(0.6) )
                else if ( valueString.startsWith( "track" ) )           				Some( Score(0.9) )
                else if ( valueString.startsWith( "service" ) && (footAnnotation==Some("yes") || footAnnotation==Some("permissive")) )
                                                                        				Some( Score(0.9) )
                else if ( valueString.startsWith( "bridleway" ) )       				Some( Score(1.0) )
                else if ( valueString.startsWith( "footway" ) )         				Some( Score(1.0) )
                else if ( valueString.startsWith( "footpath" ) )        				Some( Score(1.0) )
                else None
            }
            case None => None
        }
        
        val costMultiplier = costMultiplierOption.getOrElse( Score(0.0) )
    
	    val sc = scenicScore( re )
	    
	    costMultiplier * sc * Score(re.landCoverScore)
	}
    
    override def speed( re : RouteEdge ) = Speed(kph=4)
    override def respectOneWay = false
}


trait BaseCycleRouting extends RouteType
{
    def inclineScore( re : RouteEdge ) : Score
    
    override def score( re : RouteEdge ) : Score =
	{
    	val tagMap = re.wayTags
    	
        val highwayAnnotation : Option[String] = tagMap.get("highway")
        val junctionAnnotation : Option[String] = tagMap.get("junction")
        val bridgeAnnotation : Option[String] = tagMap.get("bridge")
        val nameAnnotation : Option[String] = tagMap.get("name")
        val refAnnotation : Option[String] = tagMap.get("ref")
        val footAnnotation : Option[String] = tagMap.get("foot")
    
        val costMultiplierOption = highwayAnnotation match
        {
            case Some( valueString ) =>
            {
                if ( refAnnotation.isDefined && refAnnotation.get.matches("A[0-9]+") )  Some( Score(0.2) )
                else if ( valueString.startsWith( "trunk" ) )                           Some( Score(0.2) )
                else if ( valueString.startsWith( "primary" ) )                         Some( Score(0.3) )
                else if ( valueString.startsWith( "residential" ) )                     Some( Score(0.4) )
                else if ( valueString.startsWith( "road" ) )                            Some( Score(0.6) )
                else if ( valueString.startsWith( "secondary" ) )                       Some( Score(0.7) )
                else if ( valueString.startsWith( "tertiary" ) )                        Some( Score(0.8) )
                else if ( valueString.startsWith( "unclassified" ) )                    Some( Score(0.9) )
                
                // Cycleways are normally urban and along a road, or short and dull. So score low
                else if ( valueString.startsWith( "cycleway" ) )                        Some( Score(0.4) )
                else None
            }
            case None => None
        }
        
        val costMultiplier : Score = costMultiplierOption.getOrElse( Score(0.0) )
    
	    val sc = scenicScore( re )
	    
	    
	    costMultiplier * sc * inclineScore( re ) * Score(re.landCoverScore)
	}
    
    // TODO: Modify according to incline
    override def speed( re : RouteEdge ) = Speed(kph=20)
    override def respectOneWay = true
}

class BumpyCycleRouting extends BaseCycleRouting
{
    def inclineScore( re : RouteEdge ) = Score(0.5 + (((re.absHeightDelta / re.dist)*5.0) min 0.5))
}

class IgnoreHeightCycleRouting extends BaseCycleRouting
{
    def inclineScore( re : RouteEdge ) = Score(1.0)
}

object RouteType
{
    lazy val allTypes = Map(
    	"Walking"			-> new WalkingRoute(),
    	"Cycling (normal)"	-> new IgnoreHeightCycleRouting(),
    	"Cycling (hilly)"	-> new BumpyCycleRouting(),
    	"Driving"			-> new DrivingRoute()
    )
}
