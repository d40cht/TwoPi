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
    layerMarkers = new OpenLayers.Layer.Markers("Markers");
    map.addLayer(layerMarkers);

    // Add the Layer with the GPX Track
    var lgpx = new OpenLayers.Layer.PointTrack("Lakeside cycle ride", {
        strategies: [new OpenLayers.Strategy.Fixed()],
        protocol: new OpenLayers.Protocol.HTTP({
            url: routeUrl,
            format: new OpenLayers.Format.GPX()
        }),
        style: {strokeColor: "blue", strokeWidth: 10, strokeOpacity: 0.5},
        projection: new OpenLayers.Projection("EPSG:4326")
    });
    map.addLayer(lgpx);

    var lonLat = new OpenLayers.LonLat(lon, lat).transform(new OpenLayers.Projection("EPSG:4326"), map.getProjectionObject());
    map.setCenter(lonLat, zoom);

    var size = new OpenLayers.Size(21, 25);
    var offset = new OpenLayers.Pixel(-(size.w/2), -size.h);
    var icon = new OpenLayers.Icon('http://www.openstreetmap.org/openlayers/img/marker.png',size,offset);
    var marker = new OpenLayers.Marker(lonLat,icon);
    layerMarkers.addMarker(marker);
   
    // Add a click handler
    map.events.register("click", map, function(e)
    {
        var lonLat = map.getLonLatFromPixel(e.xy);
        //marker.map = map;
        layerMarkers.removeMarker(marker);
        marker = new OpenLayers.Marker(lonLat.clone(),icon);
        layerMarkers.addMarker(marker);
        lonLat.transform(map.getProjectionObject(), new OpenLayers.Projection("EPSG:4326"));
        
        $("#lon").val( lonLat.lon );
        $("#lat").val( lonLat.lat );
    } );
}
