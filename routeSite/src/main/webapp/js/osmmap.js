// Start position for the map (hardcoded here for simplicity,
// but maybe you want to get this from the URL params)


function rememberMapPosition( map ) {
 
    var lonLat = map.getCenter().clone();
    
    lonLat.transform(map.getProjectionObject(), new OpenLayers.Projection("EPSG:4326"));
    var lon = lonLat.lon;
    var lat = lonLat.lat;
    var zoom = map.getZoom();
    
    var location = { 'lon' : lon, 'lat' : lat, 'zoom' : zoom };
    localStorage.setItem( 'mapPosition', JSON.stringify( location ) );
}


function initDefault() {
    var location = localStorage.getItem( 'mapPosition' );
    if ( location != null )
    {
        var res = JSON.parse( location );
        init( res.lon, res.lat, res.zoom, null, null );
    }
    else
    {
        init( -5.208, 54.387, 5, null, null );
    }
}

function init( lon, lat, zoom, routeUrl, gpxUrl ) {
    map = new OpenLayers.Map ("map", {
        controls:[
            new OpenLayers.Control.Navigation(),
            new OpenLayers.Control.PanZoomBar(),
            new OpenLayers.Control.LayerSwitcher(),
            new OpenLayers.Control.Attribution()],
        projection: new OpenLayers.Projection("EPSG:4326"),
        displayProjection: new OpenLayers.Projection("EPSG:4326")
    } );

    // Define the map layer
    // Here we use a predefined layer that will be kept up to date with URL changes
    
    layerMapnik = new OpenLayers.Layer.OSM.Mapnik("Mapnik");
    map.addLayer(layerMapnik);
    layerCycleMap = new OpenLayers.Layer.OSM.CycleMap("CycleMap");
    map.addLayer(layerCycleMap);
    //layerGoogle = new OpenLayers.Layer.Google("Google Streets");
    //map.addLayer(layerGoogle);
    layerMarkers = new OpenLayers.Layer.Vector("Markers", {
        eventListeners:
        {
            'featureselected':function(evt)
            {
                var feature = evt.feature;
                document.location.href = feature.attributes.url;
            },
            
            'featureunselected':function(evt)
            {
                var feature = evt.feature;
            }
        }
    });
    map.addLayer(layerMarkers);
    
    var selectCtrl = new OpenLayers.Control.SelectFeature(layerMarkers,
        {clickout: true}
    );
    map.addControl(selectCtrl);
    selectCtrl.activate();

    // Add the Layer with the GPX Track
    if ( gpxUrl != null )
    {
        var lgpx = new OpenLayers.Layer.PointTrack("Track", {
            strategies: [new OpenLayers.Strategy.Fixed()],
            protocol: new OpenLayers.Protocol.HTTP({
                url: gpxUrl,
                format: new OpenLayers.Format.GPX()
            }),
            style: {strokeColor: "blue", strokeWidth: 10, strokeOpacity: 0.5},
            projection: new OpenLayers.Projection("EPSG:4326")
        });
        map.addLayer(lgpx);
    }
    
    var addPlaceMarker = function( lonLat, imgUrl, url, zindex, width, height )
    {
        var size = new OpenLayers.Size(width, height);
        var feature = new OpenLayers.Feature.Vector(
            new OpenLayers.Geometry.Point( lonLat.lon, lonLat.lat ),
            {some:'data'},
            {externalGraphic: imgUrl, graphicHeight: size.h, graphicWidth: size.w, graphicXOffset: (-size.w/2), graphicYOffset: -size.h, graphicZIndex : zindex});
            
        
        feature.attributes = { url : url };
            
        layerMarkers.addFeatures([feature]);
        
        return feature;
    }
    
    if ( routeUrl !=  null )
    {
        var request = OpenLayers.Request.GET({
            url: routeUrl,
            callback: function(request)
            {
                var asXML = request.responseXML;
                var pics = asXML.getElementsByTagName("pic");
                for ( var i = 0; i < pics.length; i++ )
                {
                    var lon = pics[i].getAttribute("lon");
                    var lat = pics[i].getAttribute("lat");
                    var link = pics[i].getAttribute("link");
                    var icon = pics[i].getAttribute("icon");
                    
                    var lonLat = new OpenLayers.LonLat(lon, lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
                    
                    addPlaceMarker( lonLat, icon, link, 0, 20, 34 ); 
                }
                
                var pois = asXML.getElementsByTagName("poi");
                for ( var i = 0; i < pois.length; i++ )
                {
                    var lon = pois[i].getAttribute("lon");
                    var lat = pois[i].getAttribute("lat");
                    var link = pois[i].getAttribute("link");
                    var icon = pois[i].getAttribute("icon");
                    
                    var lonLat = new OpenLayers.LonLat(lon, lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
                    
                    var f = addPlaceMarker( lonLat, icon, link, 0, 28, 28 );
                    f.style.title = pois[i].getAttribute("name");
                }
            }
        });
    }

    var lonLat = new OpenLayers.LonLat(lon, lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
    map.setCenter(lonLat, zoom);
    
    map.events.register("moveend", map, function()
    {
        rememberMapPosition( map );
    } );

    
    
    var feature = null;
    
    var clickHandler = function(e)
    {
        var lonLat = map.getLonLatFromPixel(e.xy);
        if ( feature != null ) layerMarkers.removeFeatures([feature]);
        feature = addPlaceMarker( lonLat, "/img/mapMarkers/green_MarkerS.png", "Start", 1, 20, 34 );
        
        lonLat.transform(map.getProjectionObject(), new OpenLayers.Projection("EPSG:4326"));
        
        $("#lon").val( lonLat.lon );
        $("#lat").val( lonLat.lat );
    }
   
    // Add a click handler
    map.events.register("click", map, clickHandler );
}
