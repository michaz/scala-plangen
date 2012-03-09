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


object LatitudeResource extends SessionVar[Box[GoogleAccessProtectedResource]](Empty)

object CodeReceiver {

  def render = {
    val codeBox: Box[String] = S.param("code")
    "#code *" #> (codeBox match {
      case Full(c) => {
        val jacksonFactory: JacksonFactory = new JacksonFactory()
        val clientId: String = "608965655114.apps.googleusercontent.com"
        val clientSecret: String = "JsO2PpoY0CjOxzJtHeidWKYW"
        val response: AccessTokenResponse = new GoogleAuthorizationCodeGrant(
          new NetHttpTransport(),
          jacksonFactory,
          clientId,
          clientSecret,
          c,
          "http://localhost:8080/Callback").execute()
        val googleAccessProtectedResource = new GoogleAccessProtectedResource(
          response.accessToken,
          new NetHttpTransport(),
          jacksonFactory,
          clientId,
          clientSecret,
          response.refreshToken)
        LatitudeResource.set(Full(googleAccessProtectedResource))
        "Thank you. You are now logged in. Access token is: " + response.accessToken
      }
      case _: EmptyBox => "No code."
    })
  }

}