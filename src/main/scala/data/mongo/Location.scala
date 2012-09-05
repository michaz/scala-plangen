package data.mongo
import org.bson.types.ObjectId
import java.util.Date
import net.liftweb.mongodb._
import com.mongodb._
import net.liftweb.mongodb.BsonDSL._

import scala.collection.JavaConversions._
import net.liftweb.common.Logger
import bootstrap.liftweb.CurrentUser
import net.liftweb.json.JsonAST.{JValue, JObject}
import org.geotools.referencing.GeodeticCalculator
import org.joda.time.LocalDate


case class LatLong(lat: Double, long: Double) {

}

object LatLong {

  def calcDistance(first: LatLong, second: LatLong) = {
    val gc = new GeodeticCalculator()
    gc.setStartingGeographicPoint(first.long, first.lat)
    gc.setDestinationGeographicPoint(second.long, second.lat)
    gc.getOrthodromicDistance
  }


}

object Location extends MongoDocumentMeta[Location] with Logger {
  override def collectionName = "locations"
  override def formats = super.formats + new ObjectIdSerializer + new DateSerializer

  def findDays = MongoDB.use( f = db => {
    val userId = CurrentUser.is.openTheBox.currentUserId
    val params = new BasicDBObject()
    params.put("ns", "locations")
    val cond = new BasicDBObject()
    cond.put("user_id", userId)
    params.put("cond", cond)
    params.put("$reduce", "function(obj,prev) {  }")
    params.put("initial", new BasicDBObject())
    params.put("$keyf", "function(loc) { d = ISODate(\"1970-01-01\"); d.setFullYear(loc.timestamp.getFullYear()); d.setMonth(loc.timestamp.getMonth()); d.setDate(loc.timestamp.getDate()); return {\"day\": d } } ")
    val command = new BasicDBObject()
    command.put("group", params)
    val result = db.command(command)
    val retval = result.get("retval").asInstanceOf[BasicDBList]
    val days = retval.map(doc => new LocalDate(doc.asInstanceOf[BasicDBObject].get("day").asInstanceOf[Date]))
    days.sortWith(_.isBefore(_))
  })

  def findByDay(timestamp: LocalDate) = {
    val thismorning = new Date(timestamp.toDateMidnight.toDate.getTime)
    val tonight = new Date(timestamp.toDateMidnight.toDate.getTime + 60 * 60 * 24 * 1000)
    info("Fetching locations from %s to %s from database.".format(thismorning, tonight))
    val userId = CurrentUser.is.openTheBox.currentUserId
    val results = Location
      .findAll( ("timestamp" -> ("$gte" -> thismorning) ~ ("$lt" -> tonight)) ~ ("user_id" -> userId) )
      .sortBy(_.timestamp.getTime)
    results.foreach(trace(_))
    results
  }

}

case class Location(_id: ObjectId, user_id: String, location: LatLong, timestamp: Date, raw: JValue) extends MongoDocument[Location] {
  def meta = Location
}