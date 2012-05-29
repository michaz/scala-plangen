package data.mongo
import org.bson.types.ObjectId
import java.util.Date
import net.liftweb.mongodb._
import com.mongodb._
import net.liftweb.mongodb.BsonDSL._

import scala.collection.JavaConversions._

case class LatLong(lat: Double, long: Double)

object Location extends MongoDocumentMeta[Location] {
  override def collectionName = "locations"
  override def formats = super.formats + new ObjectIdSerializer + new DateSerializer

  def findDays = MongoDB.use( f = db => {
    val params = new BasicDBObject()
    params.put("ns", "locations")
    params.put("$reduce", "function(obj,prev) {  }")
    params.put("initial", new BasicDBObject())
    params.put("$keyf", "function(loc) { d = ISODate(\"1970-01-01\"); d.setFullYear(loc.timestamp.getFullYear()); d.setMonth(loc.timestamp.getMonth()); d.setDate(loc.timestamp.getDate()); return {\"day\": d } } ")
    val command = new BasicDBObject()
    command.put("group", params)
    val result = db.command(command)
    val retval = result.get("retval").asInstanceOf[BasicDBList]
    val days = retval.map(doc => doc.asInstanceOf[BasicDBObject].get("day"))
    days
  })

  def findByDay(timestamp: Date) = {
    println(timestamp)
    val tonight = new Date(timestamp.getTime + 60 * 60 * 24 * 1000)
    println(tonight)
    val results = Location.findAll(("timestamp" -> ("$gte" -> timestamp) ~ ("$lt" -> tonight)))
    results.foreach(println(_))
    results
  }

}

case class Location(_id: ObjectId, location: LatLong, timestamp: Date) extends MongoDocument[Location] {
  def meta = Location
}