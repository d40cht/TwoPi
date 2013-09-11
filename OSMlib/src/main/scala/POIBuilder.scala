package org.seacourt.osm.poi

import org.seacourt.osm._
import org.seacourt.osm.route.{RTreeIndex}

import scala.collection.{mutable, immutable}

import java.io.{File}

import org.seacourt.osm.route.{WikiLocated, POIType, POI}

object POITypes
{
    object Unclassified extends POIType
    {
        def name = "Unclassified"
        def icon = new File( "point_of_interest.png" )
    }

    object Shelter extends POIType
    {
        def name = "Shelter/Bothy"
        def icon = new File("shelter.png")
    }

    object SurveyPoint extends POIType
    {
        def name = "Survey/trig point"
        def icon = new File("survey_point.png")
    }

    object Cafe extends POIType
    {
        def name = "Cafe"
        def icon = new File("cafe.png")
    }

    object Pub extends POIType
    {
        def name = "Pub"
        def icon = new File("pub.png")
    }

    object Peak extends POIType
    {
        def name = "Peak"
        def icon = new File("peak.png")
    }

    object Cave extends POIType
    {
        def name = "Cave"
        def icon = new File("cave.png")
    }

    object Archaeological extends POIType
    {
        def name = "Archaeological"
        def icon = new File("archaeological.png")
    }
    
    object Memorial extends POIType
    {
        def name = "Memorial"
        def icon = new File("memorial.png")
    }
    
    object Ruins extends POIType
    {
        def name = "Ruins"
        def icon = new File("ruins.png")
    }

    object Monument extends POIType
    {
        def name = "Monument"
        def icon = new File("monument.png")
    }

    object Museum extends POIType
    {
        def name = "Museum"
        def icon = new File("museum.png")
    }
    
    object Historic extends POIType
    {
        def name = "Historic"
        def icon = new File("historic.png")
    }

    object Fuel extends POIType
    {
        def name = "Fuel"
        def icon = new File("fuel.png")
    }

    object Parking extends POIType
    {
        def name = "Parking"
        def icon = new File("parking.png")
    }
    
    object Viewpoint extends POIType
    {
        def name = "Viewpoint"
        def icon = new File("viewpoint.png")
    }
    
    object Place extends POIType
    {
        def name = "Place"
        def icon = new File("place.png")
    }
    
    object Natural extends POIType
    {
        def name = "Natural"
        def icon = new File("natural.png")
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



object POIBuilder extends Logging
{
    
    
    private def getWikiLocations( fileName : java.io.File ) : Seq[(String, Coord)] =
    {
        import java.io._
        import org.apache.commons.compress.compressors.bzip2._
        
        val lats = mutable.HashMap[String, Double]()
        val lons = mutable.HashMap[String, Double]()
        val ioSource = new BZip2CompressorInputStream( 
            new BufferedInputStream(
            new FileInputStream( fileName ) ) )
            
        io.Source.fromInputStream( ioSource ).getLines.foreach
        { l =>
        
            val els = l.split('^').head.split(" ").map( _.trim.drop(1).dropRight(1) )
            if ( els.size == 3 )
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
            new FileInputStream( fileName ) ) )
            
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
            new FileInputStream( fileName ) ) )
            
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
    
    private def extractLocatedWikiArticles( dbpediaCoordFile : File, dbpediaImageFile : File, dbpediaTypeFile : File ) : Seq[WikiLocated] =
    {
        val coords = getWikiLocations( dbpediaCoordFile )
        val imageMap = getWikiImages( dbpediaImageFile )
        val typeMap = getWikiTypes( dbpediaTypeFile )
        
        coords.map
        { case (name, coord) =>
            
            WikiLocated( name, coord, imageMap.get( name ), typeMap( name ) )
        }
    }
    
    private def POITypeFromOSMTags( tags : Map[String, String] ) : POIType =
    {
        tags.get("amenity") match
        {
            case Some("pub")                => return POITypes.Pub
            case Some("cafe")               => return POITypes.Cafe
            case Some("parking")            => return POITypes.Parking
            case _                          =>
        }
        
        tags.get("historic") match
        {
            case Some("archaeological_site")    => return POITypes.Archaeological
            case Some("memorial")               => return POITypes.Memorial
            case Some("monument")               => return POITypes.Monument
            case Some("ruins")                  => return POITypes.Ruins
            case _                              => return POITypes.Historic
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
            case _                          => return POITypes.Natural
        }
        
        tags.get("place") match
        {
            case _                          => return POITypes.Place
        }
        
        return POITypes.Unclassified
    }
    
    def build( map : OSMMap ) : Seq[POI] =
    {
        val dbpediaCoordFile = new java.io.File( "data/geo_coordinates_en.nt.bz2" )
        val dbpediaImageFile = new java.io.File( "data/images_en.nt.bz2" )
        val dbpediaTypeFile = new java.io.File( "data/instance_types_en.nt.bz2" )
        val dbpediaAbstractFile = new java.io.File( "data/short_abstracts_en.nt.bz2" )
        
        log.info( "Ingesting dbpedia data" )
        val wikiLocated = extractLocatedWikiArticles( dbpediaCoordFile, dbpediaImageFile, dbpediaTypeFile )
        
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
        val pois = map.poiNodes.map
        { n =>
            
            val tm = n.tagMap
            val poiType = POITypeFromOSMTags( tm )
            val wikiLinked = wikiAssocInverted.get(n)
            
            new POI( n.coord, tm("name"), poiType, wikiLinked )
        }
        
        pois
    }
}
