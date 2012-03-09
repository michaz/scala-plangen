package code.oauth;

import com.google.api.client.googleapis.auth.oauth2.draft10.GoogleAccessProtectedResource;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.jackson.JacksonFactory;
import com.google.api.services.latitude.Latitude;
import com.google.api.services.latitude.model.Location;

import java.io.IOException;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: zilske
 * Date: 11/18/11
 * Time: 4:52 PM
 * To change this template use File | Settings | File Templates.
 */
public class LatWrapper {

    private Latitude latitude;

    public LatWrapper(GoogleAccessProtectedResource s) {
        latitude = new Latitude(new NetHttpTransport(), s, new JacksonFactory());
    }

    public List<Location> getLatitude() throws IOException {
        return latitude.location.list().setGranularity("best").execute().getItems();
    }



}
