package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.Logger
import net.liftweb.http.{RequestVar, FileParamHolder, SHtml}
import xml.XML
import gpx.ParseKml
import org.joda.time.DateTime
import gpx.ParseKml.WhenWhere

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
    info("Uploaded file. " + uploadedFile.is.fileName + " " + uploadedFile.is.mimeType)
    val tracks = XML.load(uploadedFile.is.fileStream) \\ "Track"
    info("Found " + tracks.size + " tracks in KML.")
    tracks.foreach(track => {
      val readings = ParseKml.parseTrack(track)
      readings.foreach(info(_))
      readings.sortBy( l => l.when.toDate.getTime )
    })

    // if processing fails, just allow this method to exit to re-render your
    // file upload form
  }

  def render = {
    "#filename" #> SHtml.fileUpload(file => {
      uploadedFile.set(file)
    }) &
      "#submit" #> SHtml.submit("Upload", processUpload _ )
  }

}
