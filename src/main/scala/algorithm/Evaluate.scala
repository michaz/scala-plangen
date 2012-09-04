package algorithm

import algorithm.Labeller._
import data.mongo.Location
import algorithm.Labeller.Segmentation
import algorithm.Labeller.Facility

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 9/3/12
 * Time: 8:12 PM
 * To change this template use File | Settings | File Templates.
 */
object Evaluate {


  val days = Location.findDays.toList
  var facilitiesFromEvaluation = Seq[Facility]()
  var labellingsFromEvaluation = Seq[LabelledSegment]()

  (0 to 1).foreach { _ =>
    labellingsFromEvaluation = (for (day <- days) yield {
      val locations = Location.findByDay(day)
      val Segmentation(segments, distanceToNext) = segment(locations)
      evaluate(segments)
    }).flatten
    facilitiesFromEvaluation = deriveFacilities(labellingsFromEvaluation.toList.filter(_.isActivity))
    println("Number of facilities: "+ facilitiesFromEvaluation.size)
  }

  def evaluate(segments: Seq[Segment]) = {
    val instance = Learn.constructInstance(segments.map(LabelledSegment(_, false)), facilitiesFromEvaluation)
    val labelsInCrappyType = Learn.acrf.getBestLabels(instance)
    val labels = (0 to labelsInCrappyType.size-1).map(labelsInCrappyType.getLabels(_))
    for ((segment, label) <- segments zip labels) yield {
      val actOrLeg = label.get(0).toString
      LabelledSegment(segment, if(actOrLeg == "act") true else if(actOrLeg == "leg") false else throw new RuntimeException(actOrLeg) )
    }
  }

}
