package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import com.mongodb.Mongo
import net.liftweb.mongodb.{DefaultMongoIdentifier, MongoDB}
import code.snippet.LatitudeResource
import code.oauth.OAuth2ClientCredentials
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeRequestUrl
import com.google.api.services.latitude.LatitudeScopes
import java.text.SimpleDateFormat
import java.util.{Date, Arrays}

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  def boot {
    


    MongoDB.defineDb(DefaultMongoIdentifier, new Mongo, "test")
    
    // where to search snippet
    LiftRules.addToPackages("code")

    val url = new GoogleAuthorizationCodeRequestUrl(OAuth2ClientCredentials.CLIENT_ID, "http://localhost:8080/Callback", Arrays.asList(LatitudeScopes.LATITUDE_ALL_BEST, "https://www.googleapis.com/auth/userinfo.profile")).build()

    val loggedInToLatitude = If(() => LatitudeResource.is.isDefined,
                                () => RedirectResponse(url))

    val notLoggedInToLatitude = If(() => LatitudeResource.is.isEmpty,
                                   () => RedirectResponse("/"))


    // val menu = Menu.param[ParamInfo]("Days", "Days", s => Full(ParamInfo(s)), pi => pi.theParam) / "locations" / "day" / "index"
    // das erscheint einfach nicht!!

    case class ParamInfo(theParam: String)

    // Build SiteMap
    val sitemap = SiteMap(
      Menu.i("Home") / "index", // the simple way to declare a menu
      Menu(Loc("Callback", "Callback" :: Nil, "Callback", Hidden)),
      Menu(Loc("Import from Latitude", "google_map" :: "today" :: Nil, "Today", loggedInToLatitude, EarlyResponse(()=>Full(RedirectResponse("/google_map/" + theDateFormat.format(new Date)))))),
      Menu(Loc("Browse Latitude", "google_map" :: Nil, "Browse Latitude", loggedInToLatitude, Hidden)),
      Menu(Loc("Log-in to Latitude", ExtLink(url), "Log-in to Latitude", notLoggedInToLatitude)),
      Menu.i("Browse database") / "locations" / "day" / "index" >> loggedInToLatitude,
      Menu.i("List") / "locations" / "list_locations" >> loggedInToLatitude >> Hidden,
      Menu.i("Map in database") / "locations" / "google_database_map" >> loggedInToLatitude >> Hidden,
      Menu.i("Logout") / "logout" >> loggedInToLatitude >> EarlyResponse(()=> {
        LatitudeResource.remove()
        Full(RedirectResponse("/"))
      }))



    LiftRules.setSiteMap(sitemap)

    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("locations", "day", date, "index"),_,_,_),_,_) if date != "index" =>
        RewriteResponse("locations":: "list_locations" :: Nil, Map("date" -> date))
      case RewriteRequest(ParsePath(List("locations", "day", date, "map"),_,_,_),_,_) if date != "index" =>
        RewriteResponse("locations" :: "google_database_map" :: Nil, Map("date" -> date))
      case RewriteRequest(ParsePath(List("google_map", date),_,_,_),_,_) if date != "today" =>
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
