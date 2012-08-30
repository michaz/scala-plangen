package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import net.liftweb.mongodb.{DefaultMongoIdentifier, MongoDB}
import net.liftweb.json._
import com.google.api.services.latitude.LatitudeScopes
import java.text.SimpleDateFormat
import java.util.{Date, Arrays}
import io.Source
import com.mongodb.{ServerAddress, Mongo}
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.googleapis.auth.oauth2.{GoogleCredential, GoogleAuthorizationCodeTokenRequest, GoogleAuthorizationCodeRequestUrl}
import com.google.api.client.auth.oauth2.Credential
import service.User
import com.google.api.services.oauth2.Oauth2


case class OAuth2ClientCredentials(CLIENT_ID: String, CLIENT_SECRET: String)

object OAuth2ClientCredentials extends OAuth2ClientCredentials("608965655114-3tm53d29c3dan6nna8eif98chq24dm60.apps.googleusercontent.com", "KgSRkmAsz9WeYmAqO7zvOV_z")

object LatitudeResource extends RequestVar[Box[Credential]](Empty)

object CurrentUser extends SessionVar[Box[User]](Empty)

object MyBoot {  // Grauenhaft!! Wie geht das richtig?
  var baseUrl: String = ""
  // where to search snippet

  LiftRules.addToPackages("code")
  def googleUrl(localUrl: String) = new GoogleAuthorizationCodeRequestUrl(
    OAuth2ClientCredentials.CLIENT_ID,
    baseUrl + "Callback", //TODO: Rausfinden, wie man callback an dieser Stelle auflösen kann.
    Arrays.asList(LatitudeScopes.LATITUDE_ALL_BEST)
  ).setState(localUrl).build()

}

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  implicit val formats = DefaultFormats

  case class DotcloudEnvironment(DOTCLOUD_WWW_HTTP_URL: String,
                                 DOTCLOUD_DATA_MONGODB_HOST: String,
                                 DOTCLOUD_DATA_MONGODB_PORT: String,
                                 DOTCLOUD_DATA_MONGODB_LOGIN: String,
                                 DOTCLOUD_DATA_MONGODB_PASSWORD: String)

  def boot {

    val baseUrl = try {
      val m = JsonParser.parse(Source.fromFile("/home/dotcloud/environment.json").mkString).extract[DotcloudEnvironment]
      val server = new ServerAddress(m.DOTCLOUD_DATA_MONGODB_HOST, m.DOTCLOUD_DATA_MONGODB_PORT.toInt)
      // vorher muss noch per hand auf der mongo-instanz der user "root" zur db "test" hinzugefügt werden:
      // use admin
      // db.auth("root", "3OuQRGSknxvXaugAM6xC")
      // use test
      // db.addUser("root", "3OuQRGSknxvXaugAM6xC")
      // TODO: Besser machen (anderer User, andere db, automatisch anlegen falls noch nicht vorhanden)
      MongoDB.defineDbAuth(DefaultMongoIdentifier, new Mongo(server), "test", m.DOTCLOUD_DATA_MONGODB_LOGIN, m.DOTCLOUD_DATA_MONGODB_PASSWORD)
      m.DOTCLOUD_WWW_HTTP_URL
    } catch {
      case _ => {
        MongoDB.defineDb(DefaultMongoIdentifier, new Mongo, "test")
        "http://localhost:8080/"
      }
    }

    MyBoot.baseUrl = baseUrl

    def googleLoginUrl = new GoogleAuthorizationCodeRequestUrl(
      OAuth2ClientCredentials.CLIENT_ID,
      baseUrl + "Callback", //TODO: Rausfinden, wie man callback an dieser Stelle auflösen kann.
      Arrays.asList("https://www.googleapis.com/auth/userinfo.profile")
    ).setState("/login").build()

    val callback: Loc[Unit] = Loc("Callback", "Callback" :: Nil, "Callback", Hidden
      , EarlyResponse(() => {
        S.param("code") match {
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
              baseUrl+"Callback").execute()
            val credential = new GoogleCredential.Builder()
              .setClientSecrets(clientId, clientSecret)
              .setJsonFactory(new JacksonFactory())
              .setTransport(new NetHttpTransport())
              .build().setFromTokenResponse(tokenResponse)


            S.param("state") match {
              case Full("/login") => {
                CurrentUser.set(Full(new User(Oauth2.builder(new NetHttpTransport(), new JacksonFactory()).setHttpRequestInitializer(credential).build.userinfo.get.execute.getId)))
                Full(RedirectResponse("/"))
              }
              case Full(other) => Full(RedirectWithState(other, RedirectState(()=>LatitudeResource.set(Full(credential)))))
              case _: EmptyBox => Full(RedirectResponse("/"))
            }

          }
          case _ => {
            Empty
          }
        }
      })
    )



    val loggedIn = If(() => CurrentUser.is.isDefined,
      () => RedirectResponse(MyBoot.googleUrl("/")))

    val notLoggedIn = If(() => CurrentUser.is.isEmpty,
      () => RedirectResponse("/"))


    // val menu = Menu.param[ParamInfo]("Days", "Days", s => Full(ParamInfo(s)), pi => pi.theParam) / "locations" / "day" / "index"
    // das erscheint einfach nicht!!

    case class ParamInfo(theParam: String)

    // Build SiteMap
    val sitemap = SiteMap(
      Menu.i("Home") / "index", // the simple way to declare a menu
      Menu(callback),
      Menu(Loc("Import from Latitude", "google_map" :: "today" :: Nil, "Import from Latitude", loggedIn, EarlyResponse(() => Full(RedirectResponse(MyBoot.googleUrl("/google_map/" + theDateFormat.format(new Date))))))),
      Menu(Loc("Browse Latitude", "google_map" :: Nil, "Browse Latitude", loggedIn, Hidden)),

      Menu(Loc("Log-in via Google", ExtLink(googleLoginUrl), "Log-in via Google", notLoggedIn)),
      Menu.i("Browse database") / "locations" / "day" / "index" >> loggedIn,
      Menu.i("List") / "locations" / "list_locations" >> loggedIn >> Hidden,
      Menu.i("Upload KML file") / "upload_trace" >> loggedIn,
      Menu.i("Map in database") / "locations" / "google_database_map" >> loggedIn >> Hidden,
      Menu.i("Plan") / "locations" / "plan" >> loggedIn >> Hidden,
      Menu.i("Logout") / "logout" >> loggedIn >> EarlyResponse(() => {
        CurrentUser.remove()
        Full(RedirectResponse("/"))
      }))



    LiftRules.setSiteMap(sitemap)

    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("locations", "day", date, "index"), _, _, _), _, _) if date != "index" =>
        RewriteResponse("locations" :: "list_locations" :: Nil, Map("date" -> date))
      case RewriteRequest(ParsePath(List("locations", "day", date, "map"), _, _, _), _, _) if date != "index" =>
        RewriteResponse("locations" :: "google_database_map" :: Nil, Map("date" -> date))
      case RewriteRequest(ParsePath(List("locations", "day", date, "plan"), _, _, _), _, _) if date != "index" =>
        RewriteResponse("locations" :: "plan" :: Nil, Map("date" -> date))
      case RewriteRequest(ParsePath(List("google_map", date), _, _, _), _, _) if date != "today" =>
        RewriteResponse("google_map" :: Nil, Map("date" -> date))
    }

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // where to search snippet
    LiftRules.addToPackages("code")

    // Use jQuery 1.4
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))


  }

}
