package code {
package snippet {

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import java.util.Date
import Helpers._
import js.JsCmds._
import js.JE.{JsRaw, JsArray}
import js.JsCmds.JsCrVar
import js.{JsObj, JE, JsCmd}
import JE._
import _root_.scala.xml.{NodeSeq, Text}

import com.google.api.client.googleapis.auth.oauth2.draft10.GoogleAuthorizationRequestUrl
import code.oauth.OAuth2ClientCredentials

class HelloWorld extends Logger {

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
