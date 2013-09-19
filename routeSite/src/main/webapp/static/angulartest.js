
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

function buildMap( mapId )
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
    
    var lon = -5.208;
    var lat = 54.387;
    var zoom = 5;
    var lonLat = new OpenLayers.LonLat(lon, lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
    map.setCenter(lonLat, zoom);
    
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

function RouteController($scope, $log)
{
    $scope.routingPreferences = ["Walking", "Cycling"];
    
    $scope.distance = Number(localStorageGetOrElse('distance', 25.0));
    $scope.routingPreference = localStorageGetOrElse("routingPreference", $scope.routingPreferences[0] );
    
    localStorageWatch( $scope, 'distance' );
    localStorageWatch( $scope, 'routingPreference' );
    
    //var m = buildMap( "map" );
    
    //$('#routeMethodsTab a:last').tab('show')
}
