package code.snippet

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import java.util.Date
import Helpers._
import js.JE.{JsObj, JsRaw, JsArray}
import js.JsCmds._
import js.JsCmds.JsCrVar
import _root_.scala.xml.NodeSeq
import js.{JsCmds, JsObj, JsCmd}
import java.text.SimpleDateFormat
import org.bson.types.ObjectId
import data.mongo.{LatLong, Location}
import service.{Latitude, User}

import net.liftweb.json.JsonParser._
import bootstrap.liftweb.{CurrentUser, MyBoot, Boot}
import org.joda.time.LocalDate


class GoogleMap extends Logger {

  var date = new Date()

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  var locations: Seq[Location] = Nil

  def render = {
    S.param("date") match {
      case Full(dateParam) => date = theDateFormat.parse(dateParam)
      case _ => date = new Date
    }
    warn("Entering map rendering.")
    val userId = CurrentUser.is.openTheBox.currentUserId
    locations = Latitude.getLatitude(date.getTime)
      // .filter(jsonLoc => jsonLoc.getAccuracy != null)
      .map(jsonLoc => {
      info(jsonLoc.toPrettyString)
      val loc = Location(
        ObjectId.get,
        userId,
        LatLong(jsonLoc.getLatitude().asInstanceOf[java.math.BigDecimal].doubleValue(), jsonLoc.getLongitude().asInstanceOf[java.math.BigDecimal].doubleValue()),
        new Date(jsonLoc.getTimestampMs().asInstanceOf[String].toLong),
        parse(jsonLoc.toPrettyString))
      loc
    })
    info("Got " + locations.size + " locations.")
    renderGoogleMap()
  }

  // render the google map
  def renderGoogleMap(): NodeSeq = {
    val jsLocations: Seq[JsObj] = locations.map(loc => {
      makeLocation(loc.timestamp.toString, loc.location.lat.toString, loc.location.long.toString)
    })

    (<head>
      {Script(OnLoad(ajaxFunc(jsLocations)))}
    </head>)
  }

  def renderButton = {
    "#gestern [href]" #> MyBoot.googleUrl("/google_map/" + theDateFormat.format(new Date(date.getTime - 24 * 60 * 60 * 1000))) &
      "#morgen [href]" #> MyBoot.googleUrl("/google_map/" + theDateFormat.format(new Date(date.getTime + 24 * 60 * 60 * 1000))) &
      "#date" #> SHtml.text(theDateFormat.format(date), content => JsCmds.RedirectTo(MyBoot.googleUrl(("/google_map/" + content)))) &
      "#submit [onClick]" #> SHtml.ajaxInvoke(() => {
        Location.findByDay(new LocalDate(date)).foreach(_.delete)
        locations.foreach(_.save)
      })
  }

  // converts a the location into a JSON Object
  def makeLocation(title: String, lat: String, lng: String): JsObj = {
    JsObj(("title", title),
      ("lat", lat),
      ("lng", lng))
  }

  // called by renderGoogleMap which passes the list of locations
  // into the javascript function as json objects
  def ajaxFunc(locobj: Seq[JsObj]): JsCmd = {
    JsCrVar("locations", JsObj(("loc", JsArray(locobj: _*)))) & JsRaw("drawmap(locations)").cmd
  }

}

