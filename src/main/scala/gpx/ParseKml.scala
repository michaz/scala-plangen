package gpx

import org.joda.time.DateTime
import xml.{NodeSeq, XML}
import org.joda.time.format.ISODateTimeFormat
import data.mongo.LatLong

object ParseKml {

  private def dateTimeFormatter = ISODateTimeFormat.dateTime()

  def readFile = {
    println("Reading file")
    val xml = XML.loadString(scala.io.Source.fromFile("/Users/zilske/Downloads/history-06-27-2012.kml").mkString)
    assert(xml.isInstanceOf[scala.xml.Elem])
    println("Done")
    (xml \\ "Track").foreach(parseTrack _)
  }
  case class WhenWhere(when: DateTime, coord: String)

  def parseTrack(track: NodeSeq) = {
    var when = new DateTime()
    val timesAndCoordsAndEmpties = track match {
      case <gx:Track>{trackdata @ _*}</gx:Track> => for (data <- trackdata) yield data match {
        case <when>{whendata}</when> => when = dateTimeFormatter.parseDateTime(whendata.toString); None
        case <gx:coord>{coorddata}</gx:coord> => Some(WhenWhere(when, coorddata.toString))
        case _ => None
      }
    }
    timesAndCoordsAndEmpties.flatten
  }


}



