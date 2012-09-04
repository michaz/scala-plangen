package algorithm

import java.util.Date
import net.liftweb.util.ValueCell
import data.mongo.{TruthRecord, TruthItem}
import org.joda.time.{DateTime, Interval, LocalDate}
import org.bson.types.ObjectId
import bootstrap.liftweb.CurrentUser
import algorithm.Labeller.{LabelledSegment, Segment}

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 9/4/12
 * Time: 10:53 AM
 * To change this template use File | Settings | File Templates.
 */
class Truth(date: Date) {

  val contents = ValueCell[Vector[TruthItem]](Vector())

  def addItem(item: TruthItem) {
    contents.atomicUpdate(v => v :+ item)
    save
  }

  def removeItem(item: TruthItem) {
    contents.atomicUpdate(v => v.filterNot(_ == item))
    save
  }

  def clear {
    contents.atomicUpdate(v => Vector())
  }

  def isMergeable(item: TruthItem) = {
    true
  }

  def merge(item: TruthItem) {
    val i = contents.indexOf(item)
    val prev = contents(i-1)
    val next = contents(i+1)
    val merged = TruthItem(prev.from, next.to, if(prev.tag == next.tag) prev.tag else prev.tag + "/" + next.tag)
    contents.atomicUpdate(v => {
      val firstHalf = v.take(i-1)
      val secondHalf = v.drop(i+2)
      firstHalf ++ (merged +: secondHalf)
    })
    save
  }

  def load {
    // not atomic
    contents.atomicUpdate (v => {
      var temp = Vector[TruthItem]()
      for (truthRecord <- TruthRecord.findByDay(new LocalDate(date))) {
        temp = temp :+ truthRecord.ti
      }
      temp
    })
    println(contents)
  }

  def save {
    var oldTruth = TruthRecord.findByDay(new LocalDate(date))
    for (truthItem <- oldTruth) {
      truthItem.delete
    }
    for (truthItem <- contents) {
      val tr = TruthRecord(ObjectId.get, CurrentUser.is.openTheBox.currentUserId, truthItem)
      println(tr)
      tr.save
    }
  }

}

object Truth {

  def labelWithTruth(labelling: Seq[Segment], truth: Seq[TruthItem]): List[LabelledSegment] = {
    (for (segment <- labelling) yield {
      val inTrueActivity = truth.filter(_.tag == "act") exists {
        t =>
          new Interval(new DateTime(segment.startTime), new DateTime(segment.endTime))
            .overlaps(new Interval(new DateTime(t.from), new DateTime(t.to)))
      }
      LabelledSegment(segment, inTrueActivity)
    }).toList
  }

}
