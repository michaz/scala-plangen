package code.snippet

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import Helpers._
import js.JE.{JsObj, JsRaw, JsArray}
import js.JsCmds._
import js.JsCmds.JsCrVar
import js.{JsCmds, JsObj, JsCmd}
import data.mongo.Location
import java.util.Date
import java.text.SimpleDateFormat
import xml.NodeSeq
import service.User
import bootstrap.liftweb.CurrentUser
import org.joda.time.LocalDate

class GoogleDatabaseMap extends Logger {

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  var date = new Date

  var locations: List[Location] = Nil

  def render = {
    println(S.param("date"))
    S.param("date") match {
      case Full(dateParam) => date = theDateFormat.parse(dateParam)
      case _ => ()
    }
    locations = Location.findByDay(new LocalDate(date))
    renderGoogleMap()
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

  // render the google map
  def renderGoogleMap(): NodeSeq = {

    warn("Entering map rendering.")

    // setup some locations to display on the map
    // val jsLocations: List[JsObj] = List(makeLocation("loc1","40.744715", "-74.0046"),makeLocation("loc2","40.75684", "-73.9966"))

    val jsLocations: Seq[JsObj] = locations.map(loc => {
      makeLocation(loc.timestamp.toString, loc.location.lat.toString, loc.location.long.toString)
    })

    // where the magic happens
    (<head>
      {Script(OnLoad(ajaxFunc(jsLocations)))}
    </head>)
  }

  def renderButton = {
    "#gestern [href]" #> ("/locations/day/" + theDateFormat.format(new Date(date.getTime - 24 *60*60*1000)) + "/map") &
    "#morgen [href]" #> ("/locations/day/" + theDateFormat.format(new Date(date.getTime + 24 *60*60*1000)) + "/map") &
    "#date" #> SHtml.text(theDateFormat.format(date), content => JsCmds.RedirectTo("/locations/day/" + content + "/map")) &
    "#delete_all [onClick]" #> SHtml.ajaxInvoke(()=>{
      CurrentUser.is.openTheBox.findAllLocations.foreach(_.delete)
      JsCmds.Alert("Done")
    }) &
    "#delete_day [onClick]" #> SHtml.ajaxInvoke(() => {
      Location.findByDay(new LocalDate(date)).foreach(_.delete)
      List(JsCmds.Alert("Done"), JsCmds.RedirectTo("/locations/day/"))
    })
  }


}