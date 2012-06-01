package code.snippet

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._
import com.google.api.client.googleapis.auth.oauth2.draft10.GoogleAccessTokenRequest.GoogleAuthorizationCodeGrant
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.auth.oauth2.draft10.AccessTokenResponse
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.googleapis.auth.oauth2.draft10.GoogleAccessProtectedResource
import net.liftweb.http.{SessionVar, S}
import java.lang.String
import code.oauth.OAuth2ClientCredentials
import com.google.api.client.googleapis.auth.oauth2.{GoogleCredential, GoogleAuthorizationCodeTokenRequest}
import com.google.api.client.auth.oauth2.Credential


object LatitudeResource extends SessionVar[Box[Credential]](Empty)

object CodeReceiver {

  def render = {
    val codeBox: Box[String] = S.param("code")
    "#code *" #> (codeBox match {
      case Full(c) => {
        val jacksonFactory: JacksonFactory = new JacksonFactory()
        val clientId: String = OAuth2ClientCredentials.CLIENT_ID
        val clientSecret: String = OAuth2ClientCredentials.CLIENT_SECRET
        val tokenResponse = new GoogleAuthorizationCodeTokenRequest(
          new NetHttpTransport(),
          jacksonFactory,
          clientId,
          clientSecret,
          c,
          "http://localhost:8080/Callback").execute()


        val credential = new GoogleCredential.Builder()
          .setClientSecrets(clientId, clientSecret)
          .setJsonFactory(new JacksonFactory())
          .setTransport(new NetHttpTransport())
          .build().setFromTokenResponse(tokenResponse)



        LatitudeResource.set(Full(credential))

        "Thank you. You are now logged in. Access token is: " + tokenResponse.getAccessToken
      }
      case _: EmptyBox => "No code."
    })
  }

}