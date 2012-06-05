package service

import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.http.javanet.NetHttpTransport
import bootstrap.liftweb.LatitudeResource
import com.google.api.services.latitude.model.Location
import scala.collection.JavaConversions._

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 6/5/12
 * Time: 12:27 PM
 * To change this template use File | Settings | File Templates.
 */

object Latitude {

  def getLatitude(mintime: Long) = {
    val credentials = LatitudeResource.is.openTheBox
    val latitude = com.google.api.services.latitude.Latitude.builder(new NetHttpTransport(), new JacksonFactory()).setHttpRequestInitializer(credentials).build();
    val maybeLocations = Option(latitude.location.list.setMinTime(mintime.toString).setMaxTime((mintime + 24 * 60 * 60 * 1000).toString).setGranularity("best").execute().getItems)
    val result: Seq[Location] = maybeLocations match {
      case None => Nil
      case Some(locations) => List(locations:_*)
    }
    result
  }

}
