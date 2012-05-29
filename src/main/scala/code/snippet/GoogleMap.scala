package code.snippet

import scala.collection.JavaConversions._
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import java.util.Date
import Helpers._
import js.JE.{JsObj, JsRaw, JsArray}
import js.JsCmds._
import js.JsCmds.JsCrVar
import _root_.scala.xml.{NodeSeq, Text}
import code.oauth.LatWrapper
import js.{JsCmds, JsObj, JE, JsCmd}
import java.text.SimpleDateFormat
import org.bson.types.ObjectId
import data.mongo.{LatLong, Location}


class GoogleMap extends Logger {

  var date = new Date()

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  var locations: Seq[Location] = Nil

  def render = {
    println(S.param("date"))
    S.param("date") match {
      case Full(dateParam) => date = theDateFormat.parse(dateParam)
      case _ => date = new Date
    }
    warn("Entering map rendering.")
    val latitude = new LatWrapper(LatitudeResource.is.openTheBox)
    locations = latitude.getLatitude(date.getTime).map(jsonLoc => Location(
      ObjectId.get,
      LatLong(jsonLoc.getLatitude().asInstanceOf[java.math.BigDecimal].doubleValue(), jsonLoc.getLongitude().asInstanceOf[java.math.BigDecimal].doubleValue()),
      new Date(jsonLoc.getTimestampMs().asInstanceOf[String].toLong)))
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
      "#gestern [href]" #> ("/google_map/" + theDateFormat.format(new Date(date.getTime - 24 *60*60*1000))) &
      "#morgen [href]" #> ("/google_map/" + theDateFormat.format(new Date(date.getTime + 24 *60*60*1000))) &
      "#date" #> SHtml.text(theDateFormat.format(date), content => JsCmds.RedirectTo("/google_map/" + content)) &
      "#submit [onClick]" #> SHtml.ajaxInvoke(()=>{
        val latitude = new LatWrapper(LatitudeResource.is.openTheBox)
        locations.foreach(_.save)
       })
  }
}

