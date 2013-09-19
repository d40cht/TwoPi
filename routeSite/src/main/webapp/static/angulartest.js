
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

function ManagedMarker( layer, lonLat, imgUrl, url, zindex, width, height )
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
        this.removeMarker();
        this.marker = createPlaceMarker( newLonLat, imgUrl, url, zindex, width, height );
        layer.addFeatures([this.marker]);
    };
}

function buildMap( scope, mapId, lon, lat, zoom )
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
    
    var layerMarkers = new OpenLayers.Layer.Vector("Markers");
    map.addLayer(layerMarkers);
    
    var feature = new ManagedMarker( layerMarkers, lonLat, "/img/mapMarkers/green_MarkerS.png", "Start", 1, 20, 34 );
    var clickHandler = function(e)
    {
        if ( scope.currentField != null )
        {
            var lonLat = map.getLonLatFromPixel(e.xy);
            feature.moveMarker( lonLat );
            
            lonLat.transform(map.getProjectionObject(), new OpenLayers.Projection("EPSG:4326"));
            
            scope[scope.currentField] = lonLat.lon.toFixed(6) + "," + lonLat.lat.toFixed(5);
            scope.currentField = null
            scope.$apply()
        }
    }
    
    // Add a click handler
    map.events.register("click", map, clickHandler );
    
    return map;
}

function buildElevationGraph( divId, seriesData )
{
    externalSeriesData = seriesData
    $("#"+divId).highcharts({
        chart : { type : 'line' },
        title : { text : 'Elevation profile' },
        xAxis : { title : { text : 'Distance' } },
        yAxis : { title : { text : '(m)' } },
        series : [{ showInLegend: false, name : 'elevation', type : 'area', data : seriesData }],
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
                            var lonLat = new OpenLayers.LonLat(this.lon, this.lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
                            moveMapCrossLinkMarker( lonLat );
                        }
                    }
                }
            }
        }
    });
}


function RouteController($scope, $log, $http)
{
    $scope.routingPreferences = ["Walking", "Cycling"];
    $scope.distance = Number(localStorageGetOrElse('distance', 25.0));
    $scope.routingPreference = localStorageGetOrElse("routingPreference", $scope.routingPreferences[0] );
    
    var mapLon = Number(localStorageGetOrElse("mapLon", -5.208 ));
    var mapLat = Number(localStorageGetOrElse("mapLat", 54.387 ));
    var mapZoom = Number(localStorageGetOrElse("mapZoom", 5 ));
    
    localStorageWatch( $scope, 'distance' );
    localStorageWatch( $scope, 'routingPreference' );
    
    
    var eg = buildElevationGraph("elevation", []);
    var map = buildMap($scope, "map", mapLon, mapLat, mapZoom);
    
    
    $scope.currentField = null;
    $scope.setStart = function()
    {
        $scope.startCoord = "";
        $scope.currentField = "startCoord";
    };
    
    $scope.setMid = function()
    {
        $scope.midCoord = "";
        $scope.currentField = "midCoord";
    };
    
    $scope.routeWithStart = function()
    {
        var dist = Number($scope.distance);
        var start = $scope.startCoord.split(",");
        var lon = Number(start[0]);
        var lat = Number(start[1]);
        
        var params = $.param(
        {
            distance : dist,
            lon: lon,
            lat: lat
        } );
        
        $http( {
            method: "POST",
            url : "/requestroute",
            headers: {'Content-Type': 'application/x-www-form-urlencoded'},
            data : params
        } )
        .success( function(data, status, headers, config )
        {
            alert( "Success: " + data );
        } )
        .error( function(data, status, headers, config )
        {
            alert( "Failure: " + data );
        } );
    };
}


