

angular.module('TwoPi', ['ngCookies'], function($provide)
    {
        $provide.factory( 'UserService', function($cookies) {
            var sdo =
            {
                userName : $cookies['UserName']
	        };
	        return sdo;
        } );
        
        $provide.factory('RouteStateService', function() {
            var routeStateKey = 'routeState';
            
            var state = null;
            var ls = localStorage.getItem( routeStateKey );
            if ( ls == null )
            {
                state =
                {
                };
                
                localStorage.setItem( routeStateKey, JSON.stringify(state) );
            }
            else
            {
                state = JSON.parse(ls);
            }
            
            var sdo =
            {
                getState : function()
                {
                    return state;
                },
                
                saveState : function()
                {
                    localStorage.setItem( routeStateKey, JSON.stringify(state) );
                }
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
            .when('/poster/:routeId',
            {
                templateUrl : '/partials/poster.html',
                controller  : PosterController
            } )
            .when('/user',
            {
                templateUrl : '/partials/user.html',
                controller  : UserController
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
            marker.removeFrom(map);
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

function RouteMap( mapId, lng, lat, zoom )
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
        
        localStorage.setItem( "mapLon", lng );
        localStorage.setItem( "mapLat", lat );
        localStorage.setItem( "mapZoom", zoom );
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
            for ( rd in data.directions )
            {
                var dataEl = data.directions[rd];
                for ( picI in dataEl.inboundPics )
                {
                    var pic = dataEl.inboundPics[picI];
                    pics.push( {
                        title   : pic.title + ", " + pic.photographer,
                        imgSrc  : "/geograph/full/" + pic.picIndex + "/" + pic.imgHash,
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
            alert( "Failure in PosterController data fetch: " + status );
        } );
}


function RouteController($scope, $log, $http, $location, $routeParams, UserService)
{   
    // Pull possible routing preferences from the server
    $scope.routingPreferences = [];
    $http( {
            method  : "GET",
            url     : "/costModels"
    } )
    .success( function(data, status, headers, config )
    {
        $scope.routingPreferences = data;
    } );    
   
    
    $scope.distance = Number(localStorageGetOrElse('distance', 25.0));
    $scope.routingPreference = localStorageGetOrElse("routingPreference", $scope.routingPreferences[0] );
    
    $scope.userName = UserService.userName;
    
    var mapLon = Number(localStorageGetOrElse("mapLon", -5.208 ));
    var mapLat = Number(localStorageGetOrElse("mapLat", 54.387 ));
    var mapZoom = Number(localStorageGetOrElse("mapZoom", 5 ));
    
    localStorageWatch( $scope, 'distance' );
    localStorageWatch( $scope, 'routingPreference' );
    
    var eg = new ElevationGraph("elevation");
    var mapHolder = new RouteMap("map", mapLon, mapLat, mapZoom);
    
    var startMarker = new ManagedMarker( mapHolder.getMap(), "/img/mapMarkers/green_MarkerS.png", "Start", 1, 20, 34 );
    var midMarker = new ManagedMarker( mapHolder.getMap(), "/img/mapMarkers/green_MarkerE.png", "End", 1, 20, 34 );
    var elevationCrossLinkMarker = new ManagedMarker( mapHolder.getMap(), "/img/mapMarkers/red_MarkerE.png", "", 1, 20, 34 );
    
    $scope.mapHolder = mapHolder;
    
    $scope.startCoord = "";
    $scope.midCoord = "";
    
    $scope.setStart = function()
    {
        mapHolder.setClickCallback( function(lonLat)
        {
            startMarker.moveMarker( lonLat );
            $scope.startCoord = lonLat.lng.toFixed(6) + "," + lonLat.lat.toFixed(5);
            $scope.$apply();
        } );
    };
    
    $scope.setMid = function()
    {
        mapHolder.setClickCallback( function(lonLat)
        {
            midMarker.moveMarker( lonLat );
            $scope.midCoord = lonLat.lng.toFixed(6) + "," + lonLat.lat.toFixed(5);
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
            url     : "/saveroute/" + $scope.routeId + "/" + $scope.routeName
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
        .success( function(routeData, status, headers, config )
        {
            $scope.routeData = routeData;
            
            // Update the map and elevation graph
            var seriesData = [];
            var lastNode = null;
            var ascent = 0.0;
            var totalDistance = 0.0;
            
            var routePoints = [];
            for ( rd in routeData.directions )
            {
                var dataEl = routeData.directions[rd];
                for ( n in dataEl.inboundNodes )
                {
                    var nodeAndDist = dataEl.inboundNodes[n];
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
            }
            
            mapHolder.setRoute( L.polyline( routePoints, {color: 'blue'} ) );
            
            /*for ( dbi in routeData.debugPoints )
            {
                var db = routeData.debugPoints[dbi];
                
                var nm = new ManagedMarker( mapHolder.getMap(), "/img/mapMarkers/" + db.name + ".png", db.title, 1, 20.0 * 0.7, 34.0 * 0.7 );
                nm.moveMarker( new L.LatLng( db.coord.lat, db.coord.lon ) );
            }*/
            
            eg.setData( seriesData, function( lonLat )
            {
                elevationCrossLinkMarker.moveMarker( lonLat );
            } );
            
            
            
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
        var nscore = Math.max( Math.min( score / 2.0, 1.0 ), 0.0 );
        var red = Math.floor(nscore*255).toString(16);
        var blue = Math.floor(255 - (nscore*255)).toString(16);

        while ( blue.length < 2 ) blue = "0" + blue;
        while ( red.length < 2 ) red = "0" + red;
        return '#' + red + '00' + blue;
    }
    
    $scope.routeDebug = function()
    {
        var dist = Number($scope.distance);
        var start = $scope.startCoord;
        $http( {
            method: "GET",
            url : "/debugroute",
            params :
            {
                distance : dist,
                start : start,
                model : $scope.routingPreference
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
            for ( di in dests )
            {
                var d = dests[di];
                
                var color = colorFromScore( d.score );
                var circle = L.circle( [d.coord.lat, d.coord.lon], 40, { color : color, fillColor: color, fillOpacity: 0.8 } );
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
        var dist = Number($scope.distance);
        var start = $scope.startCoord;
       
        var params = null;
        if ( $scope.midCoord == "" )
        {
            params = $.param(
            {
                distance : dist,
                start : start,
                model : $scope.routingPreference
            } );
        }
        else
        {
            params = $.param(
            {
                distance : dist,
                start : start,
                mid : $scope.midCoord,
                model : $scope.routingPreference
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
            localStorage.setItem( 'currentRoute', hash );
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


