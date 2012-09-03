package algorithm

import data.mongo.{LatLong, TruthRecord, TruthItem, Location}
import bootstrap.liftweb.CurrentUser
import net.liftweb.common.{Empty, Full}
import service.User
import net.liftweb.http.{S, LiftSession}
import net.liftweb.util.StringHelpers
import cc.mallet.types._
import algorithm.Labeller._
import cc.mallet.util.PropertyList
import scala.Vector
import net.liftweb.common.Full
import cc.mallet.grmm.util.LabelsAssignment
import net.liftweb.common.Full
import algorithm.Labeller.LabelledSegment
import net.liftweb.common.Full
import cc.mallet.pipe.Pipe
import cc.mallet.grmm.learning.{DefaultAcrfTrainer, ACRF}
import org.joda.time.{LocalDate, DateTime, Interval}

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 8/29/12
 * Time: 6:10 PM
 * To change this template use File | Settings | File Templates.
 */
object Learn {

  val labelDict = new LabelAlphabet
  val featureAlphabet = new Alphabet
  val pipe = new Pipe(featureAlphabet, labelDict){}
  val training = new InstanceList(pipe)
  val days = Location.findDays
  val labellings = {
    (for (day <- days) yield {
      val truth = TruthRecord.findByDay(day).map(_.ti)
      if(!truth.isEmpty) {
        println(day)
        println(truth)
        val locations = Location.findByDay(day)
        val Segmentation(segments, distanceToNext) = segment(locations)
        val labelling = labelWithTruth(segments, truth)
        Some(labelling)
      } else {
        None
      }
    }).flatten
  }

  val facilitiesFromTruth = deriveFacilities(labellings.flatten.toList.filter(_.isActivity))
  println("Number of hotspots in training set: " + facilitiesFromTruth.size)

  for (labelling <- labellings) {
    val instance = constructInstance(labelling, facilitiesFromTruth)
    training.add(instance)
  }

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

  val tmpls = Seq(
    new ACRF.UnigramTemplate (0),    // Warum funktioniert das nicht, wenn nur das UnigramTemplate läuft?
    new ACRF.BigramTemplate (0)
  )

  val acrf = new ACRF (pipe, tmpls.toArray);

  val trainer = new DefaultAcrfTrainer();
  val testing = training
  trainer.train (acrf, training, null, testing, 100);


  var facilitiesFromEvaluation = Seq[Facility]()
  val labellingsFromEvaluation = for (day <- days) yield {
    val locations = Location.findByDay(day)
    val Segmentation(segments, distanceToNext) = segment(locations)
    Learn.evaluate(segments)
  }
  facilitiesFromEvaluation = deriveFacilities(labellings.flatten.toList.filter(_.isActivity))




  def constructInstance(truthItems: Seq[LabelledSegment], facilities: Seq[Facility]) = {
    var featureVectors = Vector[FeatureVector]()
    var labellings = Vector[Labels]()
    for (segment <- truthItems) {
        val label = labelDict.lookupLabel(if(segment.isActivity) "act" else "leg")
        val labelling = new Labels(Vector(label).toArray)
        labellings = labellings :+ labelling
        val featureVector: FeatureVector = computeFeatureVector(segment.segment, facilities)
        featureVectors = featureVectors :+ featureVector
    }

    val data = new FeatureVectorSequence(featureVectors.toArray)
    val target = new LabelsAssignment(new LabelsSequence(labellings.toArray))
    new Instance(data, target, null, null)
  }


  def computeFeatureVector(segment: Labeller.Segment, facilities: scala.Seq[Labeller.Facility]): FeatureVector = {
    var pl: PropertyList = null
    pl = PropertyList.add("duration", segment.minutes, pl)
    pl = PropertyList.add("checkin", if (segment.containsCheckin) 1.0 else 0.0, pl)
    // pl = PropertyList.add("number-points", segment.segment.locations.size, pl)
    val facility = Labeller.findNearFacility(segment, facilities.toList)
    if (facility.isDefined) {
      pl = PropertyList.add("distance", (Labeller.SNAP_TO_FACILITY - LatLong.calcDistance(facility.get.location, segment.locations.head.location)) / Labeller.SNAP_TO_FACILITY, pl)
    }
    val featureVector = new FeatureVector(featureAlphabet, pl, false)
    featureVector
  }

  def evaluate(segments: Seq[Segment]) = {
    val instance = constructInstance(segments.map(LabelledSegment(_, false)), facilitiesFromEvaluation)
    val labelsInCrappyType = acrf.getBestLabels(instance)
    val labels = (0 to labelsInCrappyType.size-1).map(labelsInCrappyType.getLabels(_))
    for ((segment, label) <- segments zip labels) yield {
      val actOrLeg = label.get(0).toString
      println(actOrLeg)
      LabelledSegment(segment, if(actOrLeg == "act") true else if(actOrLeg == "leg") false else throw new RuntimeException(actOrLeg) )
    }
  }

}
