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
    def name : String
    def score( ed : EdgeDest ) : Score
    def speed( ed : EdgeDest ) : Speed
    def respectOneWay : Boolean
    
    def scenicScore( ed : EdgeDest ) : Score =
    {
        val re = ed.edge
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
        rn.destinations.filter( de => !score(de).isZero )
    }
}


class DrivingRoute extends RouteType
{
    val name = "Driving"
    
    override def score( ed : EdgeDest ) : Score =
	{
	    val re = ed.edge
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
    
	    val sc = scenicScore( ed )
	    
	    costMultiplier * sc * Score(re.landCoverScore)
	}
    
    override def speed( ed : EdgeDest ) =
    {
        val re = ed.edge
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

class WalkingRoute() extends RouteType
{
    val name="Walking"
    
    override def score( ed : EdgeDest ) : Score =
	{
	    val re = ed.edge
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
                else if ( valueString.startsWith( "steps" ) )    						Some( Score(0.8) )
                else if ( valueString.startsWith( "track" ) )           				Some( Score(0.9) )
                else if ( valueString.startsWith( "service" ) && (footAnnotation==Some("yes") || footAnnotation==Some("permissive")) )
                                                                        				Some( Score(0.9) )
                else if ( valueString.startsWith( "bridleway" ) )       				Some( Score(1.0) )
                else if ( valueString.startsWith( "footway" ) )         				Some( Score(1.0) )
                else if ( valueString.startsWith( "footpath" ) )        				Some( Score(1.0) )
                else if ( valueString.startsWith( "path" ) )        				    Some( Score(1.0) )
                else None
            }
            case None => None
        }
        
        val costMultiplier = costMultiplierOption.getOrElse( Score(0.0) )
    
	    val sc = scenicScore( ed )
	    
	    costMultiplier * sc * Score(re.landCoverScore)
	}
    
    override def speed( ed : EdgeDest ) = Speed(kph=4)
    override def respectOneWay = false
}


trait BaseCycleRouting extends RouteType
{
    def inclineScore( re : RouteEdge ) : Score
    
    override def score( ed : EdgeDest ) : Score =
	{
	    val re = ed.edge
    	val tagMap = re.wayTags
    	
        val highwayAnnotation : Option[String] = tagMap.get("highway")
        val lanesAnnotation : Option[String] = tagMap.get("lanes")
        //val junctionAnnotation : Option[String] = tagMap.get("junction")
        //val bridgeAnnotation : Option[String] = tagMap.get("bridge")
        val nameAnnotation : Option[String] = tagMap.get("name")
        //val refAnnotation : Option[String] = tagMap.get("ref")
        //val footAnnotation : Option[String] = tagMap.get("foot")
        
    
        val costMultiplierOption = highwayAnnotation match
        {
            case Some( valueString ) =>
            {
                if ( valueString.startsWith( "trunk" ) )
                {
                    val multipleCarriageWay =
                        (ed.routeDirectionality != Bidirectional) &&
                        (lanesAnnotation.isDefined && lanesAnnotation != Some("1"))
                        
                    if ( multipleCarriageWay )                                          Some( Score(0.05) )
                    else                                                                Some( Score(0.2) )
                }
                else if ( valueString.startsWith( "steps" ) )    						Some( Score(0.1) )
                else if ( valueString.startsWith( "primary" ) )                         Some( Score(0.3) )
                else if ( valueString.startsWith( "residential" ) )                     Some( Score(0.4) )
                else if ( valueString.startsWith( "road" ) )                            Some( Score(0.6) )
                else if ( valueString.startsWith( "secondary" ) )                       Some( Score(0.7) )
                else if ( valueString.startsWith( "tertiary" ) )                        Some( Score(0.8) )
                else if ( valueString.startsWith( "unclassified" ) )                    Some( Score(0.9) )
                else if ( valueString.startsWith( "cycleway" ) )
                {
                    val railAnnotation : Option[String] = tagMap.get("railway")
                    val adjacent : Option[String] = tagMap.get("adjacent")
                    val segregated : Option[String] = tagMap.get("segregated")
                    val ncn_ref : Option[String] = tagMap.get("ncn_ref")
                    
                    // Abandoned railways are perfect for cycling
                    if ( railAnnotation == Some("abandoned") )                          Some( Score(1.0) )
                    // Cycleways are normally urban and along a road, or short and dull. So default score low
                    else if ( segregated == Some("no") )                                Some( Score(0.4) )
                    else if ( segregated == Some("yes") )                               Some( Score(0.6) )
                    else if ( adjacent.isDefined )                                      Some( Score(0.5) )
                    else if ( ncn_ref.isDefined )                                       Some( Score(0.7) )
                    else                                                                Some( Score(0.6) )
                }
                else None
            }
            case None => None
        }
        
        val costMultiplier : Score = costMultiplierOption.getOrElse( Score(0.0) )
    
	    val sc = scenicScore( ed )
	    
	    
	    costMultiplier * sc * inclineScore( re ) * Score(re.landCoverScore)
	}
    
    // TODO: Modify according to incline
    override def speed( ed : EdgeDest ) = Speed(kph=20)
    override def respectOneWay = true
}

class BumpyCycleRouting() extends BaseCycleRouting
{
    val name="Cycling (hilly)"
    
    def inclineScore( re : RouteEdge ) = Score(0.5 + (((Math.abs(re.forwardHeightDelta) / re.dist)*5.0) min 0.5))
}

class IgnoreHeightCycleRouting() extends BaseCycleRouting
{
    val name="Cycling (normal)"
    
    def inclineScore( re : RouteEdge ) = Score(1.0)
}

object RouteType
{
    lazy val allTypes = Seq(
        new WalkingRoute(),
        new IgnoreHeightCycleRouting(),
        new BumpyCycleRouting(),
        new DrivingRoute() )
    .map( x => (x.name, x) )
    .toMap
}
