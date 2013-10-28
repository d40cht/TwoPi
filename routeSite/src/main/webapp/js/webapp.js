

angular.module('TwoPi', ['ngCookies', 'ngStorage'], function($provide)
    {
        $provide.factory( 'UserService', function($cookies) {
            var sdo =
            {
                userName : $cookies['UserName']
	        };
	        return sdo;
        } );
    } )
    .config(['$routeProvider', '$locationProvider', function($routeProvider, $locationProvider)
    {
        $locationProvider.html5Mode(true);
        
        $routeProvider
            .when('/',
            {
                templateUrl : '/partials/makeroute.html',
                controller  : RouteController
            } )
            .when('/route/:routeId',
            {
                templateUrl : '/partials/makeroute.html',
                controller  : RouteController
            } )
            .when('/saveRoute/:routeId',
            {
                templateUrl : '/partials/saveRoute.html',
                controller  : RouteSaveController
            } )
            .when('/poster/:routeId',
            {
                templateUrl : '/partials/poster.html',
                controller  : PosterController
            } )
            .when('/user',
            {
                templateUrl : '/partials/user.html',
                controller  : UserController
            } )
            .when('/about',
            {
                templateUrl : '/partials/about.html'
            } );
    }] );


var TRACK_LAYER_INDEX = 0;
var MARKER_LAYER_INDEX = 1;


function createPlaceMarker( lonLat, imgUrl, title, zindex, width, height )
{
    var icon = L.icon( {
        iconUrl : imgUrl,
        iconSize : [width, height],
        iconAnchor : [width/2, height]
    } ); 
    
    var marker = new L.marker( lonLat, { icon: icon, zIndexOffset: zindex, title: title } );
    
    return marker;
}

function ManagedMarker( map, imgUrl, title, zindex, width, height )
{   
    var marker = null;
    
    
    this.removeMarker = function()
    {
        if ( marker != null )
        {
            map.removeLayer(marker);
            marker = null;
        }
    };
    
    this.moveMarker = function( newLonLat )
    {
        if ( marker != null )
        {
            marker.setLatLng( newLonLat );
        }
        else
        {
            marker = createPlaceMarker( newLonLat, imgUrl, title, zindex, width, height );
            marker.addTo(map);
        }
    };
}

function RouteMap( mapId, lng, lat, zoom, $scope, $log )
{
    var map = L.map(mapId).setView( [lat, lng], zoom );
    
    this.getMap = function()
    {
        return map;
    }
    
    // add an OpenStreetMap tile layer
    L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', { attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors' }).addTo(map);
    
    map.on("moveend", function(e)
    {
        var lonLat = map.getCenter();
        var zoom = map.getZoom();
        
        $log.info( "Setting local storage" );
        $scope.$storage.mapLon = lonLat.lng;
        $scope.$storage.mapLat = lonLat.lat;
        $scope.$storage.mapZoom = zoom;
        $scope.$apply();
        $log.info( "  complete" );
    } );
    
   
    var clickCallback = null;
    this.setClickCallback = function(fn)
    {
        clickCallback = fn;
    };
    
    
    map.on("click", function(e)
    {
        if ( clickCallback != null )
        {
            clickCallback( e.latlng );
        }
    } );
    
    var currentRoute = null;
    this.setRoute = function( newRoute )
    {
        if ( currentRoute != null )
        {
            currentRoute = null;
            currentRoute.removeFrom(map);
        }
        currentRoute = newRoute;
        currentRoute.addTo(map);
        map.fitBounds( newRoute.getBounds() );
    };
    
    
    this.zoomToExtent = function( bounds )
    {
        map.fitBounds( bounds );
        var maxZoom = 15;
        if ( map.getZoom() > maxZoom ) map.zoomTo(maxZoom);
    };
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
                                var lonLat = new L.LatLng( this.lat, this.lng );
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

function UserController($scope, $routeParams, $http)
{
    $http( {
            method: "GET",
            url : ("/myroutes")
        } )
        .success( function( data, status, headers, config )
        {
            $scope.myroutes = data;
        } );
}

function RouteSaveController($scope, $routeParams, $http, $location)
{
    $scope.routeId = $routeParams.routeId
    
    $http( {
            method: "GET",
            url : ("/getroutesummary/" + $scope.routeId)
        } )
        .success( function( data, status, headers, config )
        {
            $scope.routeSummary = data;
        } )
        .error( function(data, status, headers, config )
        {
            alert( "Failure in fetching route summary: " + status );
        } );

    $scope.saveRoute = function()
    {   
        var params = $.param( {
            id : $scope.routeId,
            name : $scope.name,
            description : $scope.description
        } );
        
        $http( {
            method: "POST",
            url : "/saveroute",
            headers: {'Content-Type': 'application/x-www-form-urlencoded'},
            data : params
        } )
        .success( function( data, status, headers, config )
        {
            $location.path("/route/" + $scope.routeId)
        } )
        .error( function(data, status, headers, config )
        {
            alert( "Failure in saving route summary: " + status + " - " + data );
        } );
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
            var route = data.route;
            for ( rd in route.directions )
            {
                var dataEl = route.directions[rd];
                for ( picI in dataEl.outboundPics )
                {
                    var pic = dataEl.outboundPics[picI];
                    pics.push( {
                        title   : pic.title + ", " + pic.photographer,
                        imgSrc  : "/geograph/full/" + pic.picIndex + "/" + pic.imgHash,
                        link    : "http://www.geograph.org.uk/photo/" + pic.picIndex,
                        score   : pic.score
                    } );
                }
                
                for ( poiI in dataEl.outboundPOIs )
                {
                    var poi = dataEl.outboundPOIs[poiI];
                    
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
            $scope.routeData = route;
            
            
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
            alert( "Failure in PosterController data fetch: " + status );
        } );
}




function RouteController($scope, $log, $http, $location, $localStorage, $routeParams, UserService, $timeout)
{   
    $log.info("Route controller started");
    $scope.$storage = $localStorage;
    
    $scope.$storage = $localStorage.$default(
    {
        distance            : 25.0,
        mapLon              : -5.208,
        mapLat              : 54.387,
        mapZoom             : 5,
        startCoord          : null,
        midCoord            : null,
        routeMode           : "startMode"
    } );

    // Pull possible routing preferences from the server
    $scope.routingPreferences = [];
    $http( {
            method  : "GET",
            url     : "/costModels"
    } )
    .success( function(data, status, headers, config )
    {
        $scope.routingPreferences = data;
        
        if ( !angular.isDefined( $scope.$storage.routingPreference ) )
        {
            $scope.$storage.routingPreference = data[0];
        }
    } );     
    
    
    $scope.userName = UserService.userName;
    
    $scope.lonLatToString = function( lonLat )
    {
        if ( lonLat == null ) return "";
        else return lonLat.lng.toFixed(6) + "," + lonLat.lat.toFixed(5);
    }
    
    $scope.renderRoutePoint = function( lonLat )
    {
        if ( lonLat == null ) return "Click map to select point";
        else return $scope.lonLatToString( lonLat );
    }
    
    
    var eg = new ElevationGraph("elevation");
    var mapHolder = new RouteMap("map", $scope.$storage.mapLon, $scope.$storage.mapLat, $scope.$storage.mapZoom, $scope, $log);
    
    var startMarker = new ManagedMarker( mapHolder.getMap(), "/img/mapMarkers/green_MarkerS.png", "Start", 1, 20, 34 );
    var midMarker = new ManagedMarker( mapHolder.getMap(), "/img/mapMarkers/green_MarkerE.png", "End", 1, 20, 34 );
    var elevationCrossLinkMarker = new ManagedMarker( mapHolder.getMap(), "/img/mapMarkers/red_MarkerE.png", "", 1, 20, 34 );
    
    if ( $scope.$storage.startCoord != null )
    {
        startMarker.moveMarker( $scope.$storage.startCoord );
    }
    
    if ( $scope.$storage.midCoord != null )
    {
        midMarker.moveMarker( $scope.$storage.midCoord );
    }
    
    $scope.mapHolder = mapHolder;
    
    $scope.selecting = null;
    $scope.setStart = function()
    {
        $scope.selecting="start";
        $scope.$storage.startCoord = null;
        mapHolder.setClickCallback( function(lonLat)
        {
            startMarker.moveMarker( lonLat );
            $scope.$storage.startCoord = lonLat;
            $scope.selecting=null;
            $scope.$apply();
        } );
    };
    
    $scope.setMid = function()
    {
        $scope.selecting="mid";
        mapHolder.setClickCallback( function(lonLat)
        {
            midMarker.moveMarker( lonLat );
            $scope.$storage.midCoord = lonLat;
            $scope.selecting=null;
            $scope.$apply();
        } );
    };
    
    $scope.startMode = function()
    {
        $scope.$storage.routeMode = "startMode";
        midMarker.removeMarker();
        $scope.$storage.midCoord = null;
        //$scope.setStart();
    }
    
    $scope.startEndMode = function()
    {
        $scope.$storage.routeMode = "startEndMode";
        /*if ( $scope.startCoord == null )
        {
            $scope.setStart();
        }
        else
        {
            $scope.setMid();
        }*/
    }
    
    $scope.feelLuckyMode = function()
    {
        $scope.$storage.routeMode = "feelLuckyMode";
        startMarker.removeMarker();
        midMarker.removeMarker();
        $scope.startCoord = null;
        $scope.midCoord = null;
    }
    
    // Remember the route mode
    $timeout( function()
    {
        var tabUrl = '#routeMethodsTab a[data-target="#' + $scope.$storage.routeMode + '"]';
        $log.info( "Selecting tab: " + tabUrl );
        var el = $(tabUrl);
        $log.info( el );
        el.tab('show');
        $log.info( "  complete" );
    }, 0 );
    
    
    $scope.poiIcon = function( poi )
    {
        var hasWikiLink = angular.isDefined( poi.wikiData );
        var iconName = "amenity_recycling";
        if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Parking$" ) iconName = "transport_parking_car";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Pub$" ) iconName = "food_biergarten";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Cafe$" ) iconName = "food_cafe";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Place$" ) iconName = "poi_place_town";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Peak$" ) iconName = "poi_peak2";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Archaeological$" ) iconName = "tourist_archaeological";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Ruins$" ) iconName = "tourist_ruin";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Historic$" ) iconName = "tourist_museum";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Memorial$" ) iconName = "tourist_memorial";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Fuel$" ) iconName = "transport_fuel";
        else if ( poi.poiType.jsonClass=="org.seacourt.osm.poi.POITypes$Unclassified$" ) iconName = "poi_point_of_interest";
        
        if ( hasWikiLink ) return (
        {
            hasLink : true,
            icon : "/img/poiIcons/" + iconName + ".p.16.png",
            link : "http://en.wikipedia.org/wiki/" + poi.wikiData.name
        } );
        else return (
        {
            hasLink : false,
            icon : "/img/poiIcons/" + iconName + ".p.16.png",
        } );
    }

    
    //$scope.setStart();
    
    function setRoute(routeId)
    {
        $http( {
            method: "GET",
            url : ("/getroute/" + routeId)
        } )
        .success( function(namedRoute, status, headers, config )
        {
            var routeData = namedRoute.route;
            $scope.routeData = routeData;
            if ( angular.isDefined(namedRoute.name) ) $scope.routeName = namedRoute.name;
            else $scope.routeName = null;
            
            // Update the map and elevation graph
            var seriesData = [];
            var lastNode = null;
            var ascent = 0.0;
            var totalDistance = 0.0;
            
            var routePoints = [];
            for ( rd in routeData.directions )
            {
                var dataEl = routeData.directions[rd];
                for ( n in dataEl.outboundNodes )
                {
                    var nodeAndDist = dataEl.outboundNodes[n];
                    var distance = nodeAndDist.distance / 1000.0;
                    var node = nodeAndDist.node;
                    
                    seriesData.push( { x : distance, y : node.height, lng : node.coord.lon, lat : node.coord.lat } );
                    
                    routePoints.push( new L.LatLng( node.coord.lat, node.coord.lon ) );
                    
                    if ( lastNode != null )
                    {
                        heightDelta = node.height - lastNode.height;
                        if ( heightDelta > 0.0 ) ascent += heightDelta;
                    }
                    lastNode = node;
                    totalDistance = distance;
                }
                
                for ( poii in dataEl.outboundPOIs )
                {
                    var poi = dataEl.outboundPOIs[poii];
                    $log.info( "POI: " + poi.name + " - " + angular.toJson(poi) );
                }
            }
            
            mapHolder.setRoute( L.polyline( routePoints, {color: 'blue'} ) );
            
            for ( dbi in routeData.debugPoints )
            {
                var db = routeData.debugPoints[dbi];
                
                var nm = new ManagedMarker( mapHolder.getMap(), "/img/mapMarkers/" + db.name + ".png", db.title, 1, 20.0 * 0.7, 34.0 * 0.7 );
                nm.moveMarker( new L.LatLng( db.coord.lat, db.coord.lon ) );
            }
            
            eg.setData( seriesData, function( lonLat )
            {
                elevationCrossLinkMarker.moveMarker( lonLat );
            } );
            
            // Update the directions text
            var directions = [];
            for ( si in routeData.directions )
            {
                var section = routeData.directions[si];

                var routeText = function( section )
                {
                    return section.cumulativeDistance.toFixed(1) + "km - " + section.directionsText;
                }
                
                
                
                directions.push( {
                    coord : section.coord,
                    text : routeText(section),
                    outboundPOIs : section.outboundPOIs,
                    outboundPics : section.outboundPics
                } );
            }
            
            $scope.directions = directions;
            
            
            $scope.routeId = routeId
        } )
        .error( function(data, status, headers, config )
        {
            alert( "Failure in setRoute: " + status );
        } );
    }
    
    $scope.moveMarker = function( lng, lat )
    {
        elevationCrossLinkMarker.moveMarker( new L.LatLng( lat, lng ) );
    };
    
    var cr = $routeParams['routeId'];
    if ( cr != null )
    {
        setRoute( cr );
    }
    
    var colorFromScore = function( score )
    {
        var nscore = score;
        var red = Math.floor(255 - (nscore*255)).toString(16);
        var blue = Math.floor(nscore*255).toString(16);

        while ( blue.length < 2 ) blue = "0" + blue;
        while ( red.length < 2 ) red = "0" + red;
        return '#' + red + '00' + blue;
    }
    
    
    
    $scope.routeDebug = function()
    {
        var dist = Number($scope.$storage.distance);
        var start = $scope.lonLatToString($scope.$storage.startCoord);
        $http( {
            method: "GET",
            url : "/debugroute",
            params :
            {
                distance : dist,
                start : start,
                model : $scope.$storage.routingPreference
            }
        } )
        .success( function(data, status, headers, config)
        {
            var ways = data.ways;
            for ( rdi in ways )
            {
                var rd = ways[rdi];
                //var score = rd.scenicScore;
                var score = rd.score;
                var lineColor = colorFromScore( score );
                
                var routePoints = [];
                for ( ci in rd.coords )
                {
                    var coord = rd.coords[ci];
                    routePoints.push( new L.LatLng( coord.lat, coord.lon ) );
                }
                
                var nr = L.polyline( routePoints, {color: lineColor} );
                nr.addTo(mapHolder.getMap());
            }
            
            var dests = data.dests;
            //var dests = data.scenicPoints;
            //var dests = data.pois;
            for ( di in dests )
            {
                var d = dests[di];
                
                var color = colorFromScore( d.score );
                var circle = L.circle( [d.coord.lat, d.coord.lon], 40, { color : color, fillColor: color, fillOpacity: 0.8 } );
                circle.bindPopup( d.title );
                circle.addTo( mapHolder.getMap() );
            }
        } )
        .error( function(data, status, headers, config)
        {
            alert( "Failure in route debug: " + status + ", " + data );
        } );
    }
    
    $scope.requestRoute = function()
    {
        var dist = Number($scope.$storage.distance);
        var start = $scope.lonLatToString($scope.$storage.startCoord);
       
        var params = null;
        if ( $scope.$storage.midCoord == null )
        {
            params = $.param(
            {
                distance : dist,
                start : start,
                model : $scope.$storage.routingPreference
            } );
        }
        else
        {
            params = $.param(
            {
                distance : dist,
                start : start,
                mid : $scope.lonLatToString($scope.$storage.midCoord),
                model : $scope.$storage.routingPreference
            } );
        };
        
        $http( {
            method: "POST",
            url : "/requestroute",
            headers: {'Content-Type': 'application/x-www-form-urlencoded'},
            data : params
        } )
        .success( function(data, status, headers, config)
        {
            var hash = data;
            $location.path( "/route/" + hash );
            setRoute( hash );
        } )
        .error( function(data, status, headers, config)
        {
            alert( "Failure in request route: " + status );
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


function TypeaheadCtrl($scope, $http)
{
    $scope.foundPlaces = [];
    
    $scope.populate = function()
    {
        $http(
        {
            method : 'GET',
            url : 'http://nominatim.openstreetmap.org/search/' + $scope.placename + '?format=json&countrycodes=gb'
        } )
        .success( function( data, status, headers, config )
        {
            var results = [];
            for ( placeid in data )
            {
                var place = data[placeid];
                // latmin, latmax, lonmin, lonmax
                // boundingbox=[52.548641204834,52.5488433837891,-1.81612110137939,-1.81592094898224 ]
                var boundingBox = place.boundingbox
                
                results.push( place );
            }
            $scope.foundPlaces = results;
        } );
        
        //http://nominatim.openstreetmap.org/search/appleton,%20oxfordshire?format=json
    }
    
    $scope.moveTo = function( boundingbox )
    {
        var southWest = new L.LatLng( boundingbox[0], boundingbox[3] );
        var northEast = new L.LatLng( boundingbox[1], boundingbox[2] );
        var bounds = new L.LatLngBounds(southWest, northEast);
        
        $scope.mapHolder.zoomToExtent( bounds );
    }
}


