
angular.module('TwoPi', [])
    .config(['$routeProvider', '$locationProvider', function($routeProvider, $locationProvider)
    {
        $locationProvider.html5Mode(true);
        
        $routeProvider
            .when('/app',
            {
                templateUrl : '/static/partials/makeroute.html',
                controller  : RouteController
            } )
            .when('/app/:routeId',
            {
                templateUrl : '/static/partials/makeroute.html',
                controller  : RouteController
            } )
            .when('/app/poster/:routeId',
            {
                templateUrl : '/static/partials/poster.html',
                controller  : PosterController
            } );
    }] );


var TRACK_LAYER_INDEX = 0;
var MARKER_LAYER_INDEX = 1;

function localStorageGetOrElse( name, value )
{
    var existing = localStorage.getItem(name);
    if ( existing == null )
    {
        return value;
    }
    else
    {
        return existing;
    }
}

function localStorageWatch( scope, name )
{
    scope.$watch( name, function()
    {
        localStorage.setItem( name, scope[name] );
    } );
}

function createPlaceMarker( lonLat, imgUrl, url, zindex, width, height )
{
    var size = new OpenLayers.Size(width, height);
    var feature = new OpenLayers.Feature.Vector(
        new OpenLayers.Geometry.Point( lonLat.lon, lonLat.lat ),
        {some:'data'},
        {externalGraphic: imgUrl, graphicHeight: size.h, graphicWidth: size.w, graphicXOffset: (-size.w/2), graphicYOffset: -size.h, graphicZIndex : zindex}
    );
    
    feature.attributes = { url : url };
    
    return feature;
}

function ManagedMarker( map, layer, imgUrl, url, zindex, width, height )
{   
    this.marker = null;
    
    this.removeMarker = function()
    {
        if ( this.marker != null )
        {
            layer.removeFeatures([this.marker]);
            this.marker = null;
        }
    };
    
    this.moveMarker = function( newLonLat )
    {
        var ll = newLonLat.clone();
        ll.transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
        this.removeMarker();
        this.marker = createPlaceMarker( ll, imgUrl, url, zindex, width, height );
        layer.addFeatures([this.marker]);
    };
}

function RouteMap( mapId, lon, lat, zoom )
{
    var mousePosition = new OpenLayers.Control.MousePosition();
    var map = new OpenLayers.Map( mapId, {
        controls: [
            new OpenLayers.Control.Navigation(),
            new OpenLayers.Control.PanZoomBar(),
            new OpenLayers.Control.LayerSwitcher(),
            new OpenLayers.Control.Attribution(),
            mousePosition],
        projection: new OpenLayers.Projection("EPSG:4326"),
        displayProjection: new OpenLayers.Projection("EPSG:4326")
    } );
    
    OpenLayers.Events.prototype.includeXY = true;

    // Define the map layer
    // Here we use a predefined layer that will be kept up to date with URL changes
    
    layerMapnik = new OpenLayers.Layer.OSM.Mapnik("Mapnik");
    map.addLayer(layerMapnik);
    layerCycleMap = new OpenLayers.Layer.OSM.CycleMap("CycleMap");
    map.addLayer(layerCycleMap);
    
    var lonLat = new OpenLayers.LonLat(lon, lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
    map.setCenter(lonLat, zoom);
    
    
    
    map.events.register("moveend", map, function()
    {
        var lonLat = map.getCenter().clone();
    
        lonLat.transform(map.getProjectionObject(), new OpenLayers.Projection("EPSG:4326"));
        var lon = lonLat.lon;
        var lat = lonLat.lat;
        var zoom = map.getZoom();
        
        localStorage.setItem( "mapLon", lon );
        localStorage.setItem( "mapLat", lat );
        localStorage.setItem( "mapZoom", zoom );
    } );
    
    var markerLayer = new OpenLayers.Layer.Vector("Markers");
    map.addLayer(markerLayer);
    map.setLayerIndex(markerLayer, MARKER_LAYER_INDEX);
    
    var clickCallback = null;
    
    function setClickCallback(fn)
    {
        clickCallback = fn;
    }
    this.setClickCallback = setClickCallback;
    

    function clickHandler(e)
    {
        if ( clickCallback != null )
        {
            var lonLat = map.getLonLatFromPixel(e.xy);
            lonLat.transform(map.getProjectionObject(), new OpenLayers.Projection("EPSG:4326"));
            
            clickCallback( lonLat );
        }
    }
    
    function toMapProjection( lonLat )
    {
        var llc = lonLat.clone();
        llc.transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
        return llc;
    }
    
    this.toMapProjection = toMapProjection;
    
    // Add a click handler
    map.events.register("click", map, clickHandler);
    
    this.map = map;
    this.markerLayer = markerLayer;
    
    var trackLayer = null;
    function setTrackLayer( newTrackLayer )
    {
        if ( trackLayer != null )
        {
            map.removeLayer( trackLayer );
        }
        trackLayer = newTrackLayer;
        map.addLayer(trackLayer);
        map.setLayerIndex(trackLayer, TRACK_LAYER_INDEX);
    }
    this.setTrackLayer = setTrackLayer;
}

function ElevationGraph( divId )
{
    var crossLinkFn = null;
    
    var chartElement = $("#"+divId);
    var chart = chartElement.highcharts({
        chart : { type : 'line' },
        title : { text : 'Elevation profile' },
        xAxis : { title : { text : 'Distance (km)' } },
        yAxis : { title : { text : '(m)' } },
        series : [{ showInLegend: false, name : 'elevation', type : 'area', data : [] }],
        plotOptions :
        {
            series :
            {
                marker : { enabled : false },
                point :
                {
                    events :
                    {
                        mouseOver : function()
                        {
                            if ( crossLinkFn != null )
                            {
                                var lonLat = new OpenLayers.LonLat(this.lon, this.lat);
                                crossLinkFn( lonLat );
                            }
                        }
                    }
                },
                turboThreshold : 0
            }
        }
    });
    
    this.setData = function( data, newCrossLinkFn )
    {
        chartElement.highcharts().series[0].setData( data, true );
        crossLinkFn = newCrossLinkFn;
    }
}

function PosterController($scope, $routeParams, $http, $timeout)
{
    $scope.routeId = $routeParams.routeId
    
    $http( {
            method: "GET",
            url : ("/getroute/" + $scope.routeId)
        } )
        .success( function( data, status, headers, config )
        {
            var pics = [];
            var wiki = [];
            for ( rd in data )
            {
                var dataEl = data[rd];
                for ( picI in dataEl.inboundPics )
                {
                    var pic = dataEl.inboundPics[picI];
                    pics.push( {
                        title   : pic.title + ", " + pic.photographer,
                        imgSrc  : "/geographFull/" + pic.picIndex + "/" + pic.imgHash,
                        link    : "http://www.geograph.org.uk/photo/" + pic.picIndex,
                        score   : pic.score
                    } );
                }
                
                for ( poiI in dataEl.inboundPOIs )
                {
                    var poi = dataEl.inboundPOIs[poiI];
                    
                    if ( poi.hasOwnProperty("wikiData") && poi.wikiData.hasOwnProperty("imageUrl") )
                    {
                        wiki.push( {
                            title   : "Wikipedia: " + poi.wikiData.name.replace(/_/g, " "),
                            imgSrc  : poi.wikiData.imageUrl,
                            link    : "http://en.wikipedia.org/wiki/" + poi.wikiData.name,
                            score   : 0.6
                        } );
                    }
                }
            }
            
            // Sort to get highest scoring pictures first
            var picsSorted = pics.slice(0);
            picsSorted.sort( function( a, b)
            {
                if ( a.score < b.score ) return 1;
                else if ( a.score > b.score ) return -1;
                else return 0;
            } );
            
            for ( pi in picsSorted )
            {
                var pic = picsSorted[pi];
                if ( pi < 2 ) pic.picClass = "masonrySize1";
                else if ( pi < 6 ) pic.picClass = "masonrySize2";
                else pic.picClass = "masonrySize3";
            }
            $scope.pics = picsSorted;
            $scope.wiki = wiki;
            $scope.routeData = data;
            
            
            $timeout( function()
            {
                var $container = $('#masonryContainer');
                
                $container.imagesLoaded( function()
                {
                    $container.masonry( {
                        columnWidth: 60,
                        itemSelect : '.masonryItem',
                    } );
                } );
            }, 0 );
        } )
        .error( function(data, status, headers, config )
        {
            alert( "Failure: " + data );
        } );
}


function RouteController($scope, $log, $http, $location, $routeParams)
{   
    // TODO: Record start and end coords into localStorage
    $scope.routingPreferences = ["Walking"];
    $scope.distance = Number(localStorageGetOrElse('distance', 25.0));
    $scope.routingPreference = localStorageGetOrElse("routingPreference", $scope.routingPreferences[0] );
    
    var mapLon = Number(localStorageGetOrElse("mapLon", -5.208 ));
    var mapLat = Number(localStorageGetOrElse("mapLat", 54.387 ));
    var mapZoom = Number(localStorageGetOrElse("mapZoom", 5 ));
    
    localStorageWatch( $scope, 'distance' );
    localStorageWatch( $scope, 'routingPreference' );
    
    
    $scope.startCoord = "";
    $scope.midCoord = "";
    
    
    var eg = new ElevationGraph("elevation");
    var mapHolder = new RouteMap("map", mapLon, mapLat, mapZoom);
    
    var startMarker = new ManagedMarker( mapHolder.map, mapHolder.markerLayer, "/img/mapMarkers/green_MarkerS.png", "Start", 1, 20, 34 );
    var midMarker = new ManagedMarker( mapHolder.map, mapHolder.markerLayer, "/img/mapMarkers/green_MarkerE.png", "End", 1, 20, 34 );
    var elevationCrossLinkMarker = new ManagedMarker( mapHolder.map, mapHolder.markerLayer, "/img/mapMarkers/red_MarkerE.png", "End", 1, 20, 34 );
    
    $scope.setStart = function()
    {
        mapHolder.setClickCallback( function(lonLat)
        {
            startMarker.moveMarker( lonLat );
            $scope.startCoord = lonLat.lon.toFixed(6) + "," + lonLat.lat.toFixed(5);
            $scope.$apply();
        } );
    };
    
    $scope.setMid = function()
    {
        mapHolder.setClickCallback( function(lonLat)
        {
            midMarker.moveMarker( lonLat );
            $scope.midCoord = lonLat.lon.toFixed(6) + "," + lonLat.lat.toFixed(5);
            $scope.$apply();
        } );
    };
    
    $scope.startMode = function()
    {
        midMarker.removeMarker();
        $scope.midCoord = "";
        $scope.setStart();
    }
    
    $scope.startEndMode = function()
    {
        if ( $scope.startCoord == "" )
        {
            $scope.setStart();
        }
        else
        {
            $scope.setMid();
        }
    }
    
    $scope.feelLuckyMode = function()
    {
        startMarker.removeMarker();
        midMarker.removeMarker();
        $scope.startCoord = "";
        $scope.midCoord = "";
    }
    
    $scope.saveRoute = function()
    {
        $http( {
            method  : "GET",
            url     : "/saveroute/" + $scope.routeInfo.routeId + "/" + $scope.routeName
        } )
        .success( function(data, status, headers, config )
        {
            alert( data );
        } );
    }
    
    $scope.setStart();
    
    function setRoute(routeId)
    {
        $http( {
            method: "GET",
            url : ("/getroute/" + routeId)
        } )
        .success( function(data, status, headers, config )
        {
            //alert( data );
            //var routeData = JSON.parse( data );
            var routeData = data;
            
            $scope.routeData = routeData;
            
            // Update the map
            var trackLayer = new OpenLayers.Layer.PointTrack("Track", {
                style: {strokeColor: "blue", strokeWidth: 6, strokeOpacity: 0.5},
                projection: new OpenLayers.Projection("EPSG:4326"),
                hover : true });
            
            
            // Update the elevation graph
            var seriesData = [];
            var lastNode = null;
            var lastPF = null;
            var ascent = 0.0;
            var totalDistance = 0.0;
            for ( rd in routeData )
            {
                var dataEl = data[rd];
                for ( n in dataEl.inboundNodes )
                {
                    var nodeAndDist = dataEl.inboundNodes[n];
                    var distance = nodeAndDist.distance / 1000.0;
                    var node = nodeAndDist.node;
                    
                    seriesData.push( { x : distance, y : node.height, lon : node.coord.lon, lat : node.coord.lat } );
                    
                    var rawPos = new OpenLayers.LonLat( node.coord.lon, node.coord.lat );
                    var tn = mapHolder.toMapProjection( rawPos );
                    var pf = new OpenLayers.Feature.Vector( new OpenLayers.Geometry.Point( tn.lon, tn.lat ) );
                    if ( lastPF != null )
                    {
                        heightDelta = node.height - lastNode.height;
                        if ( heightDelta > 0.0 ) ascent += heightDelta;
                        trackLayer.addNodes( [lastPF, pf] );
                    }
                    lastPF = pf;
                    lastNode = node;
                    totalDistance = distance;
                }
            }
            
            eg.setData( seriesData, function( lonLat )
            {
                elevationCrossLinkMarker.moveMarker( lonLat );
            } );
            
            mapHolder.setTrackLayer( trackLayer, TRACK_LAYER_INDEX );
            
            $scope.routeInfo =
            {
                routeId     : routeId,
                distance    : totalDistance,
                ascent      : ascent
            }
        } )
        .error( function(data, status, headers, config )
        {
            alert( "Failure: " + data );
        } );
    }
    
    $scope.moveMarker = function( lon, lat )
    {
        elevationCrossLinkMarker.moveMarker( new OpenLayers.LonLat( lon, lat ) );
    };
    
    var cr = $routeParams['routeId'];
    if ( cr != null )
    {
        setRoute( cr );
    }
    
    $scope.requestRoute = function()
    {
        var dist = Number($scope.distance);
        var start = $scope.startCoord;
       
        if ( $scope.midCoord == "" )
        {
            params = $.param(
            {
                distance : dist,
                start : start
            } );
        }
        else
        {
            params = $.param(
            {
                distance : dist,
                start : start,
                mid : $scope.midCoord
            } );
        };
        
        $http( {
            method: "POST",
            url : "/requestroute",
            headers: {'Content-Type': 'application/x-www-form-urlencoded'},
            data : params
        } )
        .success( function(data, status, headers, config )
        {
            var hash = data;
            localStorage.setItem( 'currentRoute', hash );
            $location.path( "/app/" + hash );
            setRoute( hash );
        } )
        .error( function(data, status, headers, config )
        {
            alert( "Failure: " + data );
        } );
    };
    
    $scope.hasWikiData = function(poi)
    {
        return Object.keys(poi).indexOf('wikiData') > 0;
    }
    
    $scope.hasWikiImage = function(poi)
    {
        return Object.keys(poi.wikiData).indexOf('imageUrl') > 0;
    }
}


