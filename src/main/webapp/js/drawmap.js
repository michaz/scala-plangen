function drawmap(locations) {
     var myOptions = {zoom: 12, mapTypeId: google.maps.MapTypeId.ROADMAP};
     var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
     initialLocation = new google.maps.LatLng(locations.loc[0].lat, locations.loc[0].lng);
     map.setCenter(initialLocation);
     for(i=0; i<locations.loc.length; i++) {
        var point = new google.maps.LatLng(locations.loc[i].lat,locations.loc[i].lng);
        var marker = new google.maps.Marker({
                position: point,
                title: locations.loc[i].title });
                marker.setMap(map);
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
     }
   }
