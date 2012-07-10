package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.{Full, Logger}
import java.text.SimpleDateFormat
import xml.Text
import net.liftweb.http.S
import data.mongo.{LatLong, Location}
import org.joda.time.{DateTime, Duration}
import org.matsim.core.utils.geometry.transformations.TransformationFactory
import org.matsim.core.utils.geometry.CoordImpl
import java.util


abstract class PlanElement {
  def startTime: util.Date
  def endTime: util.Date
}

case class Activity(activity: List[Location]) extends PlanElement {
  override def toString = "Act (" + startTime + "," + endTime + ") @ " + location
  override def startTime = activity.head.timestamp
  override def endTime = activity.last.timestamp
  def location = activity.head.location
}

case class Leg(activity1: List[Location], leg: List[List[Location]], activity2: List[Location]) extends PlanElement {
  override def toString = "Leg (" + startTime + "," + endTime + "): (" + activity1.last.location + "," + activity2.head.location
  override def startTime = activity1.last.timestamp
  override def endTime = activity2.head.timestamp
}

/**
 * A snippet transforms input to output... it transforms
 * templates to dynamic content.  Lift's templates can invoke
 * snippets and the snippets are resolved in many different
 * ways including "by convention".  The snippet package
 * has named snippets and those snippets can be classes
 * that are instantiated when invoked or they can be
 * objects, singletons.  Singletons are useful if there's
 * no explicit state managed in the snippet.
 */
object ShowPlan extends Logger {

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  val COORDINATE_SYSTEM = TransformationFactory.DHDN_GK4
  val t = TransformationFactory.getCoordinateTransformation("WGS84", COORDINATE_SYSTEM)

  def render = {
    println(S.param("date"))
    S.param("date") match {
      case Full(dateParam) => {
        val date = theDateFormat.parse(dateParam)
        val locations = Location.findByDay(date)
        val segmentedLocations = segmentLocations(locations)
        val actsAndLegs = legs(segmentedLocations)
        "#plan *" #> Text(actsAndLegs.toString)
      }
      case _ => {
        "#plan *" #> Text("Not logged in.")
      }
    }

  }

  def segmentLocations(locations: List[Location]): List[List[Location]] = {
    locations match {
      case head :: tail => {
        val (cluster, rest) = (head :: tail).span(location => (calcDistance(head.location, location.location) <= 70.0))
        cluster :: segmentLocations(rest)
      }
      case Nil => Nil
    }

  }

  def calcDistance(first: LatLong, second: LatLong) = {
    val (p1x, p1y) = getCoord(first)
    val (p2x, p2y) = getCoord(second)
    t.transform(new CoordImpl(p1x, p1y))
    val dx = p1x - p2x
    val dy = p1y - p2y
    scala.math.sqrt(dx*dx + dy*dy)
  }

  def isSignificant(segment: List[Location]) = {
    val startTime = segment.head.timestamp
    val endTime = segment.last.timestamp
    val minutes = new Duration(new DateTime(startTime), new DateTime(endTime)).getStandardMinutes
    minutes > 5
  }

  object LegActivityTail {
    def unapply(xs: List[List[Location]]) = {
      val (leg, rest) = xs.span(!isSignificant(_))
      rest match {
        case act :: tail if isSignificant(act)  => Some(leg,act,tail)
        case _ => None
      }
    }
  }

  def legs(xs: List[List[Location]]): List[Any] = {
    xs match {
      case act1 :: LegActivityTail(leg, act2, tail) if isSignificant(act1) => Activity(act1) :: Leg(act1, leg, act2) :: legs(act2 :: tail)
      case act :: tailWithoutAnotherActivity if isSignificant(act) => Activity(act) :: Nil
      case _ :: tail => legs(tail)
      case Nil => Nil
    }
  }


  def getCoord(location: LatLong) = {
    val LatLong(lat, long) = location
    val coord = t.transform(new CoordImpl(lat, long))
    (coord.getX, coord.getY)
  }


}
