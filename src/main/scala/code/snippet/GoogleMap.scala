package code.snippet

import scala.collection.JavaConversions._
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
import code.oauth.LatWrapper

class GoogleMap extends Logger {
  
   def render = renderGoogleMap()

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
    
    val latitude = new LatWrapper(LatitudeResource.is.openTheBox)
    
    val locations = latitude.getLatitude(SelectedDate.get) 
    
    // setup some locations to display on the map
	// val jsLocations: List[JsObj] = List(makeLocation("loc1","40.744715", "-74.0046"),makeLocation("loc2","40.75684", "-73.9966"))
	
    val jsLocations: Seq[JsObj] = locations.map((loc: com.google.api.services.latitude.model.Location) => 
      makeLocation(  loc.getTimestampMs().asInstanceOf[String], loc.getLatitude().toString, loc.getLongitude().toString))
    
	// where the magic happens
    (<head>
      {Script(OnLoad(ajaxFunc(jsLocations)))}
    </head>)
  }

}