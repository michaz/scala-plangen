package service

import com.google.api.services.oauth2.Oauth2
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson.JacksonFactory
import code.snippet.LatitudeResource
import data.mongo.Location
import net.liftweb.mongodb.BsonDSL._

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 6/1/12
 * Time: 11:00 AM
 * To change this template use File | Settings | File Templates.
 */

object User {

  def currentUserId: String = Oauth2.builder(new NetHttpTransport(), new JacksonFactory()).setHttpRequestInitializer(LatitudeResource.is.openTheBox).build.userinfo.get.execute.getId

  def findAllLocations = Location.findAll("user_id" -> currentUserId)

}
