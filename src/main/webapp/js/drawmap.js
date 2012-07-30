function drawmap(locations) {

     var myOptions = {zoom: 12, mapTypeId: google.maps.MapTypeId.ROADMAP};
     var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);

     var infowindow = new google.maps.InfoWindow();
     initialLocation = new google.maps.LatLng(locations.loc[0].lat, locations.loc[0].lng);
     map.setCenter(initialLocation);
     for(i=0; i<locations.loc.length; i++) {
        var point = new google.maps.LatLng(locations.loc[i].lat,locations.loc[i].lng);
        var marker = new google.maps.Marker({
                position: point,
                title: locations.loc[i].title });
        marker.setMap(map);
        google.maps.event.addListener(marker, 'click', (function(aMarker, aTitle) { return function(e) {
            infowindow.setContent(aTitle);
            infowindow.open(map, aMarker);
        }})(marker, locations.loc[i].title) );

     }
     for(i=0; i<locations.legs.length; i++) {
        var start = new google.maps.LatLng(locations.legs[i].points[0].lat,locations.legs[i].points[0].lng);
        var end = new google.maps.LatLng(locations.legs[i].points[1].lat,locations.legs[i].points[1].lng);
        var pathCoordinates = [start, end];
        var path = new google.maps.Polyline({
            path: pathCoordinates,
            strokeColor: "#FF0000",
            strokeOpacity: 1.0,
            strokeWeight: 2
        });
        path.setMap(map);
        google.maps.event.addListener(path, 'click', (function(aPath, aTitle) { return function(e) {
                    infowindow.setContent("Leg " + aTitle);
                    infowindow.position = e.latLng;
                    infowindow.open(map);
                }})(path, locations.legs[i].title) );
     }
   }
