package data.mongo

import java.util.Date
import net.liftweb.mongodb.{MongoDocument, ObjectIdSerializer, DateSerializer, MongoDocumentMeta}
import bootstrap.liftweb.CurrentUser
import net.liftweb.common.Logger
import org.bson.types.ObjectId

import net.liftweb.mongodb.BsonDSL._
import org.joda.time.LocalDate

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 8/27/12
 * Time: 3:19 PM
 * To change this template use File | Settings | File Templates.
 */

case class TruthItem(from: Date, to: Date, tag: String) {
  def id = from.getTime()+"_"+to.getTime()+"_"+tag
}

case class TruthRecord(_id: ObjectId, user_id: String, ti: TruthItem) extends MongoDocument[TruthRecord] {
  def meta = TruthRecord
}

object TruthRecord extends MongoDocumentMeta[TruthRecord] with Logger {
  override def collectionName = "truth"
  override def formats = super.formats + new ObjectIdSerializer + new DateSerializer

  def findByDay(timestamp: LocalDate) = {
    val thismorning = new Date(timestamp.toDateMidnight.toDate.getTime)
    val tonight = new Date(timestamp.toDateMidnight.toDate.getTime + 60 * 60 * 24 * 1000)
    trace("Fetching truth from %s to %s from database.".format(thismorning, tonight))
    val userId = CurrentUser.is.openTheBox.currentUserId
    val results = TruthRecord
      .findAll( ("ti.from" -> ("$gte" -> thismorning) ~ ("$lt" -> tonight)) ~ ("user_id" -> userId) )
      .sortBy(_.ti.from.getTime)
    results.foreach(trace(_))
    results
  }

}