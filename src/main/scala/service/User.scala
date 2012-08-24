package service

import com.google.api.services.oauth2.Oauth2
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson.JacksonFactory
import data.mongo.Location
import net.liftweb.mongodb.BsonDSL._
import com.google.api.client.auth.oauth2.Credential

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 6/1/12
 * Time: 11:00 AM
 * To change this template use File | Settings | File Templates.
 */

class User(login: Credential) {

  def currentUserId: String = {
    Oauth2.builder(new NetHttpTransport(), new JacksonFactory()).setHttpRequestInitializer(login).build.userinfo.get.execute.getId
  }

  def findAllLocations = Location.findAll("user_id" -> currentUserId).sortBy(_.timestamp.getTime)

}
