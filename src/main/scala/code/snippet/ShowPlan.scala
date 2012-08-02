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
import net.liftweb.http.js.{JsExp, JE, JsCmd, JsObj}
import net.liftweb.http.js.JE.{JsRaw, JsArray, JsObj}
import net.liftweb.http.js.JsCmds.{OnLoad, Script, JsCrVar}
import java.util.Date


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

  val SMALL = 70.0
  val SNAP_TO_FACILITY = 200.0

  case class Facility(name: String, location: LatLong)

  case class LabelledFacility(facility: Facility)

  case class Segmentation(segments: List[Segment], distanceToNext: Map[Location, Double])

  type Labelling = (List[LabelledSegment], List[LabelledFacility])


  trait Segment {
    def minutes: Long
    def locations: List[Location]
  }

  case class LabelledSegment(segment: Segment, needsFacility: Boolean, facility: Option[Facility])

  object Segment {
    implicit def toSegment(_locations: List[Location]) = new Segment {
      override def minutes = {
        val startTime = locations.head.timestamp
        val endTime = locations.last.timestamp
        val minutes = new Duration(new DateTime(startTime), new DateTime(endTime)).getStandardMinutes
        minutes
      }
      override def locations = _locations
    }
  }

  abstract class PlanElement {
    def segments: List[Segment]
    def startTime: Date
    def endTime: Date
    def minutes = new Duration(new DateTime(startTime), new DateTime(endTime)).getStandardMinutes
  }

  case class Activity(activity: LabelledSegment) extends PlanElement {
    override def toString = "Act (" + startTime + "," + endTime + ") @ " + location
    override def startTime = activity.segment.locations.head.timestamp
    override def endTime = activity.segment.locations.last.timestamp
    def location = activity.segment.locations.head.location
    override def segments = activity.segment :: Nil
  }

  case class Leg(activity1: LabelledSegment, leg: List[LabelledSegment], activity2: LabelledSegment) extends PlanElement {
    override def toString = "Leg (" + startTime + "," + endTime + "): (" + activity1.segment.locations.last.location + "," + activity2.segment.locations.head.location
    override def startTime = activity1.segment.locations.last.timestamp
    override def endTime = activity2.segment.locations.head.timestamp
    override def segments = leg.map(_.segment)
  }

  case class Other(other: List[LabelledSegment]) extends PlanElement {
    override def toString = segments.size + " other Elements."
    override def segments = other.map(_.segment)
    override def startTime = other.head.segment.locations.head.timestamp
    override def endTime = other.last.segment.locations.last.timestamp
  }

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  val COORDINATE_SYSTEM = TransformationFactory.DHDN_GK4
  val t = TransformationFactory.getCoordinateTransformation("WGS84", COORDINATE_SYSTEM)

  var actsAndLegs: List[PlanElement] = Nil

  def render = {

    def renderSegments(segments: scala.List[LabelledSegment], distanceToNext: Map[Location, Double]): List[CssSel] = {
      segments.map { segment =>
        "#locationList *" #> segment.segment.locations.map { location =>
          "#locationText *" #> Text(location.toString) &
          "#locationDist *" #> distanceToNext.get(location).getOrElse(0.0).toString
        } & "#segmentText *" #> Text(segment.segment.minutes + " Minuten langes Segment. Signifikant: " + segment.needsFacility + " Hat Facility: "+ segment.facility.isDefined)

      }
    }

    S.param("date") match {
      case Full(dateParam) => {
        val date = theDateFormat.parse(dateParam)
        val locations = Location.findByDay(date)
        val Segmentation(segments, distanceToNext) = segment(locations)

        var facilities: List[Facility] = Nil
        var labelling = label(segments, facilities)

        for (i <- 1 to 3) {
          facilities = deriveFacilities(labelling._1.filter(_.needsFacility))
          labelling = label(segments, facilities)
        }

        // actsAndLegs = toPlanElements(segmentedLocations.map(_.locations))
        actsAndLegs = toPlanElements(labelling)
        assert(actsAndLegs.map(planElement => planElement.segments).flatten.map(segment => segment.locations).flatten.size == locations.size)
        "#planList *" #> actsAndLegs.map { planElement =>
          "#planElementText *" #> planElement.toString &
            "#segmentList *" #> (planElement match {
              case Activity(segment) => renderSegments(segment :: Nil, distanceToNext)
              case Leg(act1, leg, act2) => renderSegments(leg, distanceToNext)
              case Other(other) => renderSegments(other, distanceToNext)
              case _ => Nil
            })
        }
      }
      case _ => {
        "#plan *" #> Text("Not logged in.")
      }
    }

  }

  def deriveFacilities(segments: List[LabelledSegment]) = {
    segments.map { segment =>
      Facility("wurst", segment.segment.locations.head.location)
    }
  }

  def label(segments: List[Segment], facilities: List[Facility]) = {
    val labelledSegments = segments.map { segment =>
      def distanceToThisSegment(facility: Facility) = calcDistance(facility.location, segment.locations.head.location)
//      val nearestFacility = facilities match {
//        case Nil => None
//        case someFacilities => {
//
//          Some(someFacilities.reduceLeft((a,b) => if (distanceToThisSegment(a) <= distanceToThisSegment(b)) a else b))
//        }
//      }

      val needsFacility = segment.minutes >= 5
      val nearestFacility = facilities.find(facility => distanceToThisSegment(facility) <= SNAP_TO_FACILITY)
      LabelledSegment(segment, needsFacility, nearestFacility)
    }
    val labelledFacilities = facilities.map { facility =>
      LabelledFacility(facility)
    }
    (labelledSegments, labelledFacilities)
  }

  def makeLeg(act1: JsObj, act2: JsObj): JsObj = {
    JsObj(("start", act1), ("end", act2))
  }

  // called by renderGoogleMap which passes the list of locations
  // into the javascript function as json objects
  def ajaxFuncDrawMap(locobj: Seq[JsObj], legobj: Seq[JsObj]): JsCmd = {
    JsCrVar("locations", JsObj(("loc", JsArray(locobj: _*)), ("legs", JsArray(legobj: _*)))) & JsRaw("drawmap(locations)").cmd
  }

  def ajaxFuncDrawChart(planElements: List[PlanElement]): JsCmd = {
    JsCrVar("data",
      JsArray(JsArray(JsExp.strToJsExp("Plan element") :: planElements.collect {
        case activity: Activity => JsExp.strToJsExp("Activity")
        case leg: Leg => JsExp.strToJsExp("Leg")
        case other: Other => JsExp.strToJsExp("Other")
      }),
      JsArray(JsExp.strToJsExp("Duration") :: planElements.collect {
        case activity: Activity => JsExp.longToJsExp(activity.minutes)
        case leg: Leg => JsExp.longToJsExp(leg.minutes)
        case other: Other => JsExp.longToJsExp(other.minutes)
      }))) & JsRaw("drawChart(data)").cmd
  }

  def renderGoogleMap = renderLocations(actsAndLegs)

  def renderLocations(planElements: List[PlanElement]): NodeSeq = {

    val jsLocations: Seq[JsObj] = planElements.collect {
      case activity: Activity => makeActivity(activity)
    }
    val jsLegs: Seq[JsObj] = planElements.collect {
      case leg: Leg => makeLeg(leg)
    }

    (<head>
      {Script(OnLoad(ajaxFuncDrawMap(jsLocations, jsLegs)))}
      {Script(OnLoad(ajaxFuncDrawChart(planElements)))}
    </head>)
  }


  def makeLeg(leg: ShowPlan.this.type#Leg): JsObj = {
    val distance = calcDistance(leg.activity1.segment.locations.head.location, leg.activity2.segment.locations.head.location)
    val duration = new Duration(new DateTime(leg.startTime), new DateTime(leg.endTime)).getStandardSeconds
    JsObj(("points", JsArray(List(leg.activity1, leg.activity2).map {
      labelledSegment => JsObj(("lat", labelledSegment.segment.locations.head.location.lat), ("lng", labelledSegment.segment.locations.head.location.long))
    })), ("title",
      new DateTime(leg.startTime).toString("HH:mm") + "-" + new DateTime(leg.endTime).toString("HH:mm") + " "
        + (distance/1000).formatted("%.2f")+"km" + " "
    + ((distance / duration) * 3.6 formatted "%.2f"+"km/h")))
  }

  def makeActivity(activity: Activity): JsObj = {
    JsObj(("title", new DateTime(activity.startTime).toString("HH:mm") + "-" + new DateTime(activity.endTime).toString("HH:mm")),
      ("lat", activity.location.lat.toString),
      ("lng", activity.location.long.toString))
  }

  def segment(locations: List[Location]): Segmentation = {
    val locationAndNext = locations.zip(locations.tail)
    val locationAndDistance = locationAndNext.map( p => (p._1, calcDistance(p._1.location,p._2.location)))
    val locationAndDistanceMap = Map() ++ locationAndDistance
    val segments = segmentLocations(locations).map(Segment.toSegment(_))
    Segmentation(segments, locationAndDistanceMap)
  }

  def segmentLocations(locations: List[Location]): List[List[Location]] = {
    locations match {
      case head :: tail => {
        val (cluster, rest) = (head :: tail).span(location => (calcDistance(head.location, location.location) <= SMALL))
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

  object LegActivityTail {
    def unapply(xs: List[LabelledSegment]) = {
      val (leg, rest) = xs.span(_.facility.isEmpty)
      rest match {
        case act :: tail if act.facility.isDefined  => Some(leg,act,tail)
        case _ => None
      }
    }
  }

  def toPlanElements(labelling: Labelling): List[PlanElement] = {
    val xs: List[LabelledSegment] = labelling._1
    toPlanElements(xs)
  }

  def toPlanElements(segments: List[LabelledSegment]): List[PlanElement] = {
    val xs = segments
    xs match {
      case act1 :: LegActivityTail(leg, act2, tail) if act1.facility.isDefined => {
        Activity(act1) :: Leg(act1, leg, act2) :: toPlanElements(act2 :: tail)
      }
      case act :: tailWithoutAnotherActivity if act.facility.isDefined => {
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
