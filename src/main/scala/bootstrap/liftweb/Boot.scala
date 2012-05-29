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
import com.google.api.client.googleapis.auth.oauth2.draft10.GoogleAuthorizationRequestUrl
import code.oauth.OAuth2ClientCredentials

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    


    MongoDB.defineDb(DefaultMongoIdentifier, new Mongo, "test")
    
    // where to search snippet
    LiftRules.addToPackages("code")

    val url = new GoogleAuthorizationRequestUrl(OAuth2ClientCredentials.CLIENT_ID, "http://localhost:8080/Callback", "https://www.googleapis.com/auth/latitude.all.best").build()


    val loggedInToLatitude = If(() => LatitudeResource.is.isDefined,
                                () => RedirectResponse(url))

    val notLoggedInToLatitude = If(() => LatitudeResource.is.isEmpty,
                                   () => RedirectResponse("/"))


    // val menu = Menu.param[ParamInfo]("Days", "Days", s => Full(ParamInfo(s)), pi => pi.theParam) / "locations" / "day" / "index"
    // das erscheint einfach nicht!!

    case class ParamInfo(theParam: String)

    // Build SiteMap
    val entries = List(
      Menu.i("Home") / "index", // the simple way to declare a menu
      Menu(Loc("Callback", "Callback" :: Nil, "Callback", Hidden)),
      Menu(Loc("Import from Latitude", "google_map" :: Nil, "Import from Latitude", loggedInToLatitude)),
      Menu(Loc("Log-in to Latitude", ExtLink(url), "Log-in to Latitude", notLoggedInToLatitude)),



      // Menu.i("Map") / "google_map",

     // Menu(Loc("Days", "locations" :: "day" :: Nil, "Days")),
    Menu.i("Days") / "locations" / "day" / "index",
      Menu.i("List") / "locations" / "list_locations",

      Menu.i("Map in database") / "locations" / "google_database_map")

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("locations", "day", date, "index"),_,_,_),_,_) if date != "index" => {
        println("sdfsdf")
        RewriteResponse("locations":: "list_locations" :: Nil, Map("date" -> date))
      }
      case RewriteRequest(ParsePath(List("locations", "day", date, "map"),_,_,_),_,_) if date != "index" =>
        RewriteResponse("locations" :: "google_database_map" :: Nil, Map("date" -> date))
      case RewriteRequest(ParsePath(List("google_map", date),_,_,_),_,_) if date != "index" =>
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
