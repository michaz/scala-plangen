package code {
package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._

import Helpers._

import com.google.api.client.googleapis.auth.oauth2.draft10.GoogleAuthorizationRequestUrl

import code.oauth.OAuth2ClientCredentials
import net.liftweb.common.{EmptyBox, Full}

class HelloWorld {

  def howdy = {
    "#time *" #> (LatitudeResource.is match {
      case Full(value) => <div>You are logged in.</div>
      case _: EmptyBox => {
        val url = new GoogleAuthorizationRequestUrl(OAuth2ClientCredentials.CLIENT_ID, "http://localhost:8080/Callback", "https://www.googleapis.com/auth/latitude.all.best").build()
        <a href={url}>Authorize</a>
      }
    })
  }

}

}
}
