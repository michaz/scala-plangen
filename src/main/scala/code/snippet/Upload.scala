package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.Logger
import net.liftweb.http.{S, RequestVar, FileParamHolder, SHtml}
import xml.XML
import gpx.ParseKml
import data.mongo.{LatLong, Location}
import org.bson.types.ObjectId
import service.User

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 7/3/12
 * Time: 3:58 PM
 * To change this template use File | Settings | File Templates.
 */

object Upload extends Logger {

  object uploadedFile extends RequestVar[FileParamHolder](null)

  def processUpload = {
    val user = User.currentUserId
    info("Uploaded file. " + uploadedFile.is.fileName + " " + uploadedFile.is.mimeType)
    val tracks = XML.load(uploadedFile.is.fileStream) \\ "Track"
    info("Found " + tracks.size + " tracks in KML.")
    var nDays = 0;
    tracks.foreach(track => {
      val readings = ParseKml.parseTrack(track)
      readings.foreach(info(_))
      readings.sortBy( l => l.when.toDate.getTime )
      val days = readings.map(_.when.toLocalDate).distinct
      days.foreach(info(_))
      days.foreach(day => Location.findByDay(day.toDateMidnight.toDate).foreach(loc => loc.delete))
      readings.foreach {reading =>
        val coords = reading.coord.split(" ").map{java.lang.Double.parseDouble(_)}
        Location(ObjectId.get, user, LatLong( coords(0), coords(1) ), reading.when.toDate).save
        nDays = nDays + 1
      }
    })
    S.notice("Thanks. I got " + nDays + " days of trajectories from you.")
    S.seeOther("/locations/day/")
  }

  def render = {
    "#filename" #> SHtml.fileUpload(file => {
      uploadedFile.set(file)
    }) &
      "#submit" #> SHtml.submit("Upload", processUpload _ )
  }

}
