package service

import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.http.javanet.NetHttpTransport
import bootstrap.liftweb.LatitudeResource
import com.google.api.services.latitude.model.Location
import scala.collection.JavaConversions._
import net.liftweb.common.Logger

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 6/5/12
 * Time: 12:27 PM
 * To change this template use File | Settings | File Templates.
 */

object Latitude extends Logger {

  def getLatitude(mintime: Long) = {
    val credentials = LatitudeResource.is.openTheBox
    val latitude = com.google.api.services.latitude.Latitude.builder(new NetHttpTransport(), new JacksonFactory()).setHttpRequestInitializer(credentials).build();

    val result = (for ((loopmintime, loopmaxtime) <- Seq((mintime, mintime + 12 * 60 * 60 * 1000 - 1), (mintime + 12 * 60 * 60 * 1000, mintime + 24 * 60 * 60 * 1000 - 1))) yield {
      val maybeLocations = Option(latitude.location
        .list
        .setMinTime(loopmintime.toString)
        .setMaxTime(loopmaxtime.toString)
        .setGranularity("best")
        .setMaxResults("10000")
        .execute()
        .getItems)
      val loopresult: Seq[Location] = maybeLocations match {
        case None => Nil
        case Some(locations) => List(locations: _*)
      }
      loopresult
    }).flatten
    info("Got " + result.size.toString + " points from Latitude.")
    result
  }

}
