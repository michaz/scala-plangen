package algorithm

import net.liftweb.http.{LiftSession, S}
import service.User
import bootstrap.liftweb.CurrentUser
import net.liftweb.common.{Empty, Full}
import data.mongo.{TruthRecord, Location}
import algorithm.Labeller._
import algorithm.Labeller.Segmentation
import net.liftweb.util.StringHelpers
import algorithm.PlanMaker.{Leg, Activity}

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 9/3/12
 * Time: 2:42 PM
 * To change this template use File | Settings | File Templates.
 */
object LearnScript extends App {

  def facilityLetters = () => {
    var nextLetter = 0
    var map = scala.collection.immutable.Map[Facility, Int]()
    def apply(f: Facility) = {
      val label = map get f match {
        case Some(label) => label
        case None => {
          val label = nextLetter
          map = map + (f -> nextLetter)
          nextLetter = nextLetter + 1
          label
        }
      }
      label
    }
    apply _
  }

  new bootstrap.liftweb.Boot().boot
  val session = new LiftSession("", StringHelpers.randomString(20), Empty)
  S.initIfUninitted(session) {
    val user = new User("111742407880819503242")
    CurrentUser.set(Full(user)) // That's me! {
    val days = Location.findDays.toList
    for (day <- days) {
      val truth = TruthRecord.findByDay(day).map(_.ti)
      if (!truth.isEmpty) {
        val locations = Location.findByDay(day)
        val Segmentation(segments, distanceToNext) = segment(locations)
        val labellingFromLearning = Evaluate.evaluate(segments)
        val backgroundFacilities = computeBackgroundFacilities(user)
        val ruleBasedLabelling = Labeller.labelWithBackground(segments, backgroundFacilities)._1
        val truthBasedLabelling = Truth.labelWithTruth(segments, truth)

        for ((name, labelling, facilities) <- Seq[(String, List[LabelledSegment], List[Facility])] (
          ("fromLearning", labellingFromLearning.toList, Evaluate.facilitiesFromEvaluation.toList),
          ("fromRules", ruleBasedLabelling, Labeller.deriveFacilities(ruleBasedLabelling.filter(s => s.isActivity))),
          ("fromTruth", truthBasedLabelling, Labeller.deriveFacilities(truthBasedLabelling.filter(s => s.isActivity))))) {
          val withNearestFacility = snapActivitiesToNearestFacility(labelling, facilities).toList
          val planElements = PlanMaker.toPlanElements(withNearestFacility)
          val myFacilityLetters = facilityLetters()
          val activityChainString = planElements.flatMap {
            case Activity(segments) => {
              Some(myFacilityLetters(segments.head.facility.get))
            }
            case _ => None
          }

          println(day + " " + name)
          println(activityChainString)
          planElements.foreach {
            case a: Activity => println("Act " + a.startTime + " " + a.endTime)
            case l: Leg => println("Leg " + l.startTime + " " + l.endTime)
            case _ => ()
          }
        }

      }
    }

  }




}
