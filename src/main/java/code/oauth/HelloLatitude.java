package code.oauth;
import com.google.api.client.googleapis.auth.oauth2.draft10.GoogleAccessProtectedResource;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.jackson.JacksonFactory;
import com.google.api.services.latitude.Latitude;


public class HelloLatitude {
	


	public static Latitude getLatitude() {
		String SCOPE = "https://www.googleapis.com/auth/latitude.all.best";
		JacksonFactory jsonFactory = new JacksonFactory();
		HttpTransport transport = new NetHttpTransport();
	    OAuth2ClientCredentials.errorIfNotSpecified();
	    GoogleAccessProtectedResource accessProtectedResource;
		try {
			accessProtectedResource = OAuth2Native.authorize(transport,
			    jsonFactory,
			    new VerificationCodeReceiver() {

					@Override
					public String getRedirectUrl() throws Exception {
						// TODO Auto-generated method stub
						return null;
					}

					@Override
					public String waitForCode() {
						// TODO Auto-generated method stub
						return null;
					}

					@Override
					public void stop() throws Exception {
						// TODO Auto-generated method stub
						
					}
				
			},
			    null,
			    "google-chrome",
			    OAuth2ClientCredentials.CLIENT_ID,
			    OAuth2ClientCredentials.CLIENT_SECRET,
			    SCOPE);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		
		Latitude latitude = new Latitude(new NetHttpTransport(), accessProtectedResource, jsonFactory);
		return latitude;
	}
	
}
