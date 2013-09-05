// Start position for the map (hardcoded here for simplicity,
// but maybe you want to get this from the URL params)



function init( lon, lat, zoom, routeUrl ) {
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
    var lgpx = new OpenLayers.Layer.PointTrack("Track", {
        strategies: [new OpenLayers.Strategy.Fixed()],
        protocol: new OpenLayers.Protocol.HTTP({
            url: routeUrl,
            format: new OpenLayers.Format.GPX()
        }),
        style: {strokeColor: "blue", strokeWidth: 10, strokeOpacity: 0.5},
        projection: new OpenLayers.Projection("EPSG:4326")
    });
    map.addLayer(lgpx);
    
    var addPlaceMarker = function( lonLat, imgUrl, url, zindex )
    {
        var size = new OpenLayers.Size(20, 34);
        var feature = new OpenLayers.Feature.Vector(
            new OpenLayers.Geometry.Point( lonLat.lon, lonLat.lat ),
            {some:'data'},
            {externalGraphic: imgUrl, graphicHeight: size.h, graphicWidth: size.w, graphicXOffset: (-size.w/2), graphicYOffset: -size.h, graphicZIndex : zindex});
            
        feature.attributes = { url : url };
            
        layerMarkers.addFeatures([feature]);
        
        return feature;
    }
    
    var request = OpenLayers.Request.GET({
        url: routeUrl,
        callback: function(request)
        {
            var asXML = request.responseXML;
            var pic = asXML.getElementsByTagName("pic");
            for ( var i = 0; i < pic.length; i++ )
            {
                var lon = pic[i].getAttribute("lon");
                var lat = pic[i].getAttribute("lat");
                var link = pic[i].getAttribute("link");
                var icon = pic[i].getAttribute("icon");
                
                var lonLat = new OpenLayers.LonLat(lon, lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
                
                //addPlaceMarker( lonLat, "http://labs.google.com/ridefinder/images/mm_20_green.png", link );
                addPlaceMarker( lonLat, icon, link, 0 ); 
            }
        }
    });

    var lonLat = new OpenLayers.LonLat(lon, lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
    map.setCenter(lonLat, zoom);

    
    
    var feature = addPlaceMarker( lonLat, "/img/mapMarkers/green_MarkerS.png", "Start", 1 );
    
    var clickHandler = function(e)
    {
        var lonLat = map.getLonLatFromPixel(e.xy);
        layerMarkers.removeFeatures([feature]);
        feature = addPlaceMarker( lonLat, "/img/mapMarkers/green_MarkerS.png", "Start", 1 );
        
        lonLat.transform(map.getProjectionObject(), new OpenLayers.Projection("EPSG:4326"));
        
        $("#lon").val( lonLat.lon );
        $("#lat").val( lonLat.lat );
    }
   
    // Add a click handler
    map.events.register("click", map, clickHandler );
}
