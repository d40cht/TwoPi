package org.seacourt.osm.poi

import org.seacourt.osm._
import org.seacourt.osm.route.{RTreeIndex}

import scala.collection.{mutable, immutable}

import java.io.{File}

import org.seacourt.osm.route.{WikiLocated, POIType, POI}
import org.seacourt.osm.Utility.{kryoCache}

object POITypes
{
    val unclassifiedIcon = new File( "img/poiIcons/shopping_estateagent.glow.20.png" )
    case object Unclassified extends POIType
    {
        def name = "Unclassified"
        def icon = unclassifiedIcon
    }

    case object Shelter extends POIType
    {
        def name = "Shelter/Bothy"
        def icon = new File("img/poiIcons/accommodation_shelter2.glow.20.png")
    }

    case object SurveyPoint extends POIType
    {
        def name = "Survey/trig point"
        def icon = new File("img/poiIcons/amenity_survey_point.glow.20.png")
    }

    case object Cafe extends POIType
    {
        def name = "Cafe"
        def icon = new File("img/poiIcons/food_cafe.glow.20.png")
    }

    case object Pub extends POIType
    {
        def name = "Pub"
        def icon = new File("img/poiIcons/food_pub.glow.20.png")
    }

    case object Peak extends POIType
    {
        def name = "Peak"
        def icon = new File("img/poiIcons/poi_peak2.glow.20.png")
    }

    case object Cave extends POIType
    {
        def name = "Cave"
        def icon = new File("img/poiIcons/poi_cave.glow.20.png")
    }

    case object Archaeological extends POIType
    {
        def name = "Archaeological"
        def icon = new File("img/poiIcons/tourist_archaeological2.glow.20.png")
    }
    
    case object Memorial extends POIType
    {
        def name = "Memorial"
        def icon = new File("img/poiIcons/tourist_memorial.glow.20.png")
    }
    
    case object Ruins extends POIType
    {
        def name = "Ruins"
        def icon = unclassifiedIcon
    }

    case object Monument extends POIType
    {
        def name = "Monument"
        def icon = new File("img/poiIcons/tourist_monument.glow.20.png")
    }

    case object Museum extends POIType
    {
        def name = "Museum"
        def icon = new File("img/poiIcons/tourist_museum.glow.20.png")
    }
    
    case object Historic extends POIType
    {
        def name = "Historic"
        def icon = unclassifiedIcon
    }

    case object Fuel extends POIType
    {
        def name = "Fuel"
        def icon = new File("img/poiIcons/transport_fuel.glow.20.png")
    }

    case object Parking extends POIType
    {
        def name = "Parking"
        def icon = new File("img/poiIcons/transport_parking_car.glow.20.png")
    }
    
    case object Viewpoint extends POIType
    {
        def name = "Viewpoint"
        def icon = new File("img/poiIcons/tourist_view_point.glow.20.png")
    }
    
    case object Place extends POIType
    {
        def name = "Place"
        def icon = new File("img/poiIcons/poi_place_hamlet.glow.20.png")
    }
    
    case object Natural extends POIType
    {
        def name = "Natural"
        def icon = new File("img/poiIcons/landuse_deciduous.glow.20.png")
    }
}


/*
http://schema.org/Museum
http://dbpedia.org/ontology/PopulatedPlace
http://dbpedia.org/ontology/NaturalPlace
http://dbpedia.org/ontology/ArchitecturalStructure
http://dbpedia.org/ontology/Infrastructure
http://dbpedia.org/ontology/MilitaryConflict
http://dbpedia.org/ontology/Event
*/

// * For wikipedia places, include if cross-linked with OSM or if rdf type in the relevant list
// * For OSM places
// * Fill in as many fields in the POI as possible



object POIBuilder extends App with Logging
{   
    private def getWikiLocations( fileName : Seq[File] ) : Seq[(String, Coord)] =
    {
        import java.io._
        import org.apache.commons.compress.compressors.bzip2._
        
        val lats = mutable.HashMap[String, Double]()
        val lons = mutable.HashMap[String, Double]()

        for ( inFile <- fileName )
        {
            val ioSource = new BZip2CompressorInputStream( 
                new BufferedInputStream(
                new FileInputStream( inFile ) ),
                true )
                
            io.Source.fromInputStream( ioSource ).getLines.foreach
            { l =>
            
                //<http://dbpedia.org/resource/Tate_St_Ives> <http://www.w3.org/2003/01/geo/wgs84_pos#lat> "50.21472222222222"^^<http://www.w3.org/2001/XMLSchema#float> .
                val els = l.split('^').head.split(" ").map( _.trim.drop(1).dropRight(1) )
                if ( els.size >= 3 )
                {
                    val name = (new java.net.URI( els(0) ) ).getPath.split("/").last
                    
                    val idName = els(1)
                    
                    idName match
                    {
                        case "http://www.w3.org/2003/01/geo/wgs84_pos#lat" => lats += (name -> els(2).toDouble)
                        case "http://www.w3.org/2003/01/geo/wgs84_pos#long" => lons += (name -> els(2).toDouble)
                        case _ =>
                    } 
                }
            }
            
            ioSource.close
        }
        
        lats.map
        { case (name, lat) =>
         
            (name, Coord(lons(name), lat))
        }
        .toSeq
    }
    
    private def getWikiImages( fileName : java.io.File ) : Map[String, String] =
    {
        import java.io._
        import org.apache.commons.compress.compressors.bzip2._
        
        val ioSource = new BZip2CompressorInputStream( 
            new BufferedInputStream(
            new FileInputStream( fileName ) ),
            true )
            
        val mapping = io.Source.fromInputStream( ioSource ).getLines.flatMap
        { l =>
        
            // <http://dbpedia.org/resource/Albedo> <http://dbpedia.org/ontology/thumbnail> <http://upload.wikimedia.org/wikipedia/commons/thumb/1/18/Albedo-e_hg.svg/200px-Albedo-e_hg.svg.png>
            val els = l.split(" ").map( _.trim.drop(1).dropRight(1) )
            
            els(1) match
            {
                case "http://dbpedia.org/ontology/thumbnail" =>
                {
                    val name = (new java.net.URI( els(0) ) ).getPath.split("/").last
                    val url = els(2)
                    
                    Some( name -> url )
                }
                case _ => None
            }
            
        }
        
        mapping.toMap
    }
    
    private def getWikiTypes( fileName : java.io.File ) : Map[String, Set[String]] =
    {
        import java.io._
        import org.apache.commons.compress.compressors.bzip2._
        
        val ioSource = new BZip2CompressorInputStream( 
            new BufferedInputStream(
            new FileInputStream( fileName ) ),
            true )
            
        var map = immutable.HashMap[String, immutable.HashSet[String]]()
        val mapping = io.Source.fromInputStream( ioSource ).getLines.foreach
        { l =>
        
            // <http://dbpedia.org/resource/Zakopane> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://dbpedia.org/ontology/Settlement>
            val els = l.split(" ").map( _.trim.drop(1).dropRight(1) )
            
            val name = (new java.net.URI( els(0) ) ).getPath.split("/").last
            val rdfType = els(2)
            
            map = map + (name -> (map.getOrElse(els(0), immutable.HashSet()) + rdfType))
        }
        
        map
    }
    
    // TODO: Pull abstract data from dbpedia too, for short article summaries
    
    private def extractLocatedWikiArticles( dbpediaCoordFiles : Seq[File], dbpediaImageFile : File, dbpediaTypeFile : File ) : Array[WikiLocated] =
    {
        val coords = getWikiLocations( dbpediaCoordFiles )
        val imageMap = getWikiImages( dbpediaImageFile )
        val typeMap = getWikiTypes( dbpediaTypeFile )
        
        coords.map
        { case (name, coord) =>
            
            WikiLocated( name, coord, imageMap.get( name ), typeMap.getOrElse( name, Set() ) )
        }
        .toArray
    }
    
    private def POITypeFromOSMTags( tags : Map[String, String] ) : POIType =
    {
        tags.get("amenity") match
        {
            case Some("pub")                => return POITypes.Pub
            case Some("cafe")               => return POITypes.Cafe
            case Some("parking")            => return POITypes.Parking
            case Some("fuel")               => return POITypes.Fuel
            case _                          =>
        }
        
        tags.get("historic") match
        {
            case Some("archaeological_site")    => return POITypes.Archaeological
            case Some("memorial")               => return POITypes.Memorial
            case Some("monument")               => return POITypes.Monument
            case Some("ruins")                  => return POITypes.Ruins
            case Some(_)                        => return POITypes.Historic
            case _                              =>
        }
        
        tags.get("tourism") match
        {
            case Some("museum")             => return POITypes.Museum
            case Some("viewpoint")          => return POITypes.Viewpoint
            case _                          =>
        }
        
        tags.get("natural") match
        {
            case Some("peak")               => return POITypes.Peak
            case Some("cave_entrance")      => return POITypes.Cave
            case Some(_)                    => return POITypes.Natural
            case _                          =>
        }
        
        tags.get("place") match
        {
            case Some(_)                    => return POITypes.Place
            case _                          =>
        }
        
        return POITypes.Unclassified
    }
    
    // For reasons unknown, mappingbased_properties is more complete than geo_coordinates for geo coordinates
    lazy val dbpediaCoordFile1 = new java.io.File( "data/geo_coordinates_en.nt.bz2" )
    lazy val dbpediaCoordFile2 = new java.io.File( "data/mappingbased_properties_en.nt.bz2" )
    lazy val dbpediaImageFile = new java.io.File( "data/images_en.nt.bz2" )
    lazy val dbpediaTypeFile = new java.io.File( "data/instance_types_en.nt.bz2" )
    lazy val dbpediaAbstractFile = new java.io.File( "data/short_abstracts_en.nt.bz2" )
    
    lazy val wikiLocatedCacheFile = new File("data/wikilocated.cache")
    
    override def main( args : Array[String] )
    {
        val lon = args(0).toDouble
        val lat = args(1).toDouble
        
        log.info( "Ingesting dbpedia data" )
        val wikiLocated = kryoCache( wikiLocatedCacheFile, extractLocatedWikiArticles( Seq(dbpediaCoordFile1, dbpediaCoordFile2), dbpediaImageFile, dbpediaTypeFile ) )
        log.info( "Number of wikipedia articles with locations: " + wikiLocated.size )
        
        log.info( "Building r-tree index" )
        val treeMap = new RTreeIndex[WikiLocated]()
        wikiLocated.foreach { case wl => treeMap.add( wl.coord, wl ) }
        
        val nearest = treeMap.nearest( Coord( lon, lat ), 20 )
        
        nearest.foreach
        { nr =>
            println( nr )
        }
    }
    
    def build( map : OSMMap ) : Seq[POI] =
    {
        log.info( "Ingesting dbpedia data" )
        val wikiLocated = kryoCache( wikiLocatedCacheFile, extractLocatedWikiArticles( Seq(dbpediaCoordFile1, dbpediaCoordFile2), dbpediaImageFile, dbpediaTypeFile ) )
        log.info( "Number of wikipedia articles with locations: " + wikiLocated.size )
        
        log.info( "Building r-tree index" )
        val treeMap = new RTreeIndex[WikiLocated]()
        wikiLocated.foreach { case wl => treeMap.add( wl.coord, wl ) }
        
        case class WikiAssociation( node : Node, geoDist : Double, wordSimilarity : Double )
        {
        }
        
        log.info( "Cross-referencing OSM and wikipedia" )
        val wikiAssoc = mutable.HashMap[WikiLocated, WikiAssociation]()
        for ( n <- map.poiNodes )
        {
            val tags = n.tags.map( t => (t.key, t.value) ).toMap
            
            //if ( !tags.contains("amenity") && !tags.contains("place") )
            {
                import com.rockymadden.stringmetric.similarity._
                
                tags.get("name") match
                {
                    case Some( name ) =>
                    {
                        def cleanWord( s : String ) = s
                            .toLowerCase
                            .replace("_"," ")
                            .replace("the", "")
                            .replace("and", "")
                            .replace("  ", " ")
                            
                        val nearest = treeMap
                            .nearest( n.coord, 10 )
                            .map( wl => (wl, wl.coord.distFrom( n.coord ), JaroWinklerMetric.compare(cleanWord(name), cleanWord(wl.name)).get ) )
                                .filter( _._2 < 200.0 )
                                .filter( _._3 >= 0.8 )
                    
                        if ( !nearest.isEmpty )
                        {
                            val (candidate, cdist, csim) = nearest.head
                            
                            if ( !wikiAssoc.contains( candidate ) )
                            {
                                wikiAssoc += (candidate -> WikiAssociation( n, cdist, csim ) )
                            }
                            else
                            {
                                val existing = wikiAssoc( candidate )
                                if ( csim > existing.wordSimilarity && cdist <= existing.geoDist )
                                {
                                    wikiAssoc(candidate) = WikiAssociation( n, cdist, csim )
                                }
                            }
                        }
                    }
                    case _ =>
                }
            }
        }
        
        val wikiAssocInverted = wikiAssoc.map { case (wl, wa) => (wa.node, wl) }
  
        log.info( "Building POIs" )
        val pois = map.poiNodes.flatMap
        { n =>
            
            val tm = n.tagMap
            val poiType = POITypeFromOSMTags( tm )
            val wikiLinked = wikiAssocInverted.get(n)
            
            if ( tm contains "name" )
            {
                Some(new POI( n.coord, tm("name"), poiType, wikiLinked ))
            }
            else None
        }
        
        pois
    }
}
