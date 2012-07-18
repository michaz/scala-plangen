package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.{Full, Logger}
import java.text.SimpleDateFormat
import xml.{NodeSeq, Text}
import net.liftweb.http.S
import data.mongo.{LatLong, Location}
import org.joda.time.{DateTime, Duration}
import org.matsim.core.utils.geometry.transformations.TransformationFactory
import org.matsim.core.utils.geometry.CoordImpl
import net.liftweb.http.js.{JsCmd, JsObj}
import net.liftweb.http.js.JE.{JsRaw, JsArray, JsObj}
import net.liftweb.http.js.JsCmds.{OnLoad, Script, JsCrVar}


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
class ShowPlan extends Logger {

  trait Segment {
    def minutes: Long
    def isSignificant: Boolean
    def locations: List[Location]
  }

  object Segment {
    implicit def toSegment(_locations: List[Location]) = new Segment {
      override def minutes = {
        val startTime = locations.head.timestamp
        val endTime = locations.last.timestamp
        val minutes = new Duration(new DateTime(startTime), new DateTime(endTime)).getStandardMinutes
        minutes
      }
      override def isSignificant = minutes > 5
      override def locations = _locations
    }
  }

  abstract class PlanElement {
    def segments: List[Segment]
  }

  case class Activity(activity: List[Location]) extends PlanElement {
    override def toString = "Act (" + startTime + "," + endTime + ") @ " + location
    def startTime = activity.head.timestamp
    def endTime = activity.last.timestamp
    def location = activity.head.location
    override def segments = Segment.toSegment(activity) :: Nil
  }

  case class Leg(activity1: List[Location], leg: List[List[Location]], activity2: List[Location]) extends PlanElement {
    override def toString = "Leg (" + startTime + "," + endTime + "): (" + activity1.last.location + "," + activity2.head.location
    def startTime = activity1.last.timestamp
    def endTime = activity2.head.timestamp
    override def segments = leg.map(Segment.toSegment(_))
  }

  case class Other(other: List[List[Location]]) extends PlanElement {
    override def toString = segments.size + " other Elements."
    override def segments = other.map(Segment.toSegment(_))
  }

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  val COORDINATE_SYSTEM = TransformationFactory.DHDN_GK4
  val t = TransformationFactory.getCoordinateTransformation("WGS84", COORDINATE_SYSTEM)

  var actsAndLegs: List[PlanElement] = Nil

  def render = {

    def renderSegments(segments: scala.List[List[Location]]): List[CssSel] = {
      segments.map { locations =>
        "#locationList *" #> locations.map { location =>
          "#locationText *" #> Text(location.toString)
        } & "#segmentText *" #> Text(Segment.toSegment(locations).minutes + " Minuten langes Segment. Signifikant: " + Segment.toSegment(locations).isSignificant)

      }
    }

    S.param("date") match {
      case Full(dateParam) => {
        val date = theDateFormat.parse(dateParam)
        val locations = Location.findByDay(date)
        val segmentedLocations = segmentLocations(locations)
        actsAndLegs = toPlanElements(segmentedLocations)
        assert(actsAndLegs.map(planElement => planElement.segments).flatten.map(segment => segment.locations).flatten.size == locations.size)
        "#planList *" #> actsAndLegs.map { planElement =>
          "#planElementText *" #> planElement.toString &
            "#segmentList *" #> (planElement match {
              case Activity(segment) => renderSegments(segment :: Nil)
              case Leg(act1, leg, act2) => renderSegments(leg)
              case Other(other) => renderSegments(other)
              case _ => Nil
            })
        }
      }
      case _ => {
        "#plan *" #> Text("Not logged in.")
      }
    }

  }

  // converts a the location into a JSON Object
  def makeLocation(title: String, lat: String, lng: String): JsObj = {
    JsObj(("title", title),
      ("lat", lat),
      ("lng", lng))
  }

  def makeLeg(act1: JsObj, act2: JsObj): JsObj = {
    JsObj(("start", act1), ("end", act2))
  }

  // called by renderGoogleMap which passes the list of locations
  // into the javascript function as json objects
  def ajaxFunc(locobj: Seq[JsObj], legobj: Seq[JsObj]): JsCmd = {
    JsCrVar("locations", JsObj(("loc", JsArray(locobj: _*)), ("legs", JsArray(legobj: _*)))) & JsRaw("drawmap(locations)").cmd
  }

  def renderGoogleMap = renderLocations(actsAndLegs)

  def renderLocations(planElements: List[PlanElement]): NodeSeq = {

    val jsLocations: Seq[JsObj] = planElements.collect {
      case activity: Activity => makeLocation(activity.endTime.toString, activity.location.lat.toString, activity.location.long.toString)
    }
    val jsLegs: Seq[JsObj] = planElements.collect {
      case leg: Leg => JsObj(("points", JsArray( List(leg.activity1, leg.activity2).map {locs => JsObj(("lat", locs.head.location.lat), ("lng", locs.head.location.long))})))
    }

    (<head>
      {Script(OnLoad(ajaxFunc(jsLocations, jsLegs)))}
    </head>)
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


  def isSignificant(segment: Segment) = {
    segment.isSignificant
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

  def toPlanElements(xs: List[List[Location]]): List[PlanElement] = {
    xs match {
      case act1 :: LegActivityTail(leg, act2, tail) if isSignificant(act1) => {
        Activity(act1) :: Leg(act1, leg, act2) :: toPlanElements(act2 :: tail)
      }
      case act :: tailWithoutAnotherActivity if isSignificant(act) => {
        Activity(act) :: toPlanElements(tailWithoutAnotherActivity)
      }
      case head::LegActivityTail(tailleg, act, tail) => {
        Other(head::tailleg) :: toPlanElements(act :: tail)
      }
      case Nil => Nil
      case onlyInsignificantStuff => {
        Other(onlyInsignificantStuff) :: Nil
      }
    }
  }


  def getCoord(location: LatLong) = {
    val LatLong(lat, long) = location
    val coord = t.transform(new CoordImpl(lat, long))
    (coord.getX, coord.getY)
  }


}
