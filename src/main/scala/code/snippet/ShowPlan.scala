package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.{Full, Logger}
import java.text.SimpleDateFormat
import xml.{NodeSeq, Text}
import net.liftweb.http.S
import data.mongo.{LatLong, Location}
import org.joda.time.{DateTime, Duration}
import net.liftweb.http.js.{JsExp, JE, JsCmd, JsObj}
import net.liftweb.http.js.JE.{JsRaw, JsArray, JsObj}
import net.liftweb.http.js.JsCmds.{OnLoad, Script, JsCrVar}
import java.util.Date
import util.Clusterer
import collection.JavaConverters._
import net.liftweb.json.JsonAST.{JNothing}
import util.Clusterer.ThingToCluster


trait Segment {
  def minutes: Long
  def locations: List[Location]
  def containsCheckin = {
    locations.exists(location => {
      val checkin = location.raw \ "activityId"
      checkin != JNothing
    })
  }
}

case class Facility(name: String, location: LatLong)

case class LabelledSegment(segment: Segment, needsFacility: Boolean, facility: Option[Facility]) extends ThingToCluster {
  def getLat = segment.locations.head.location.lat
  def getLong = segment.locations.head.location.long
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
class ShowPlan extends Logger {

  val SMALL = 50.0
  val SNAP_TO_FACILITY = 200.0


  case class LabelledFacility(facility: Facility)

  case class Segmentation(segments: List[Segment], distanceToNext: Map[Location, Double])

  type Labelling = (List[LabelledSegment], List[LabelledFacility])



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

  case class Activity(_segments: List[LabelledSegment]) extends PlanElement {
    override def toString = "Act (" + startTime + "," + endTime + ") @ " + location
    override def startTime = _segments.head.segment.locations.head.timestamp
    override def endTime = _segments.last.segment.locations.last.timestamp
    def location = _segments.head.facility.get.location
    override def segments = _segments.map(_.segment)
  }

  case class Leg(activity1: List[LabelledSegment], leg: List[LabelledSegment], activity2: List[LabelledSegment]) extends PlanElement {
    override def toString = "Leg (" + startTime + "," + endTime + "): (" + activity1.last.segment.locations.last.location + "," + activity2.head.segment.locations.head.location
    override def startTime = activity1.last.segment.locations.last.timestamp
    override def endTime = activity2.head.segment.locations.head.timestamp
    override def segments = leg.map(_.segment)
  }

  case class Other(other: List[LabelledSegment]) extends PlanElement {
    override def toString = segments.size + " other Elements."
    override def segments = other.map(_.segment)
    override def startTime = other.head.segment.locations.head.timestamp
    override def endTime = other.last.segment.locations.last.timestamp
  }

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  var actsAndLegs: Seq[PlanElement] = Nil

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

        for (i <- 1 to 1) {
          facilities = deriveFacilities(labelling._1.filter(_.needsFacility))
          labelling = label(segments, facilities)
        }

        // actsAndLegs = toPlanElements(segmentedLocations.map(_.locations))
        actsAndLegs = toPlanElements(labelling)
        assert(actsAndLegs.map(planElement => planElement.segments).flatten.map(segment => segment.locations).flatten.size == locations.size)
        "#planList *" #> actsAndLegs.map { planElement =>
          "#planElementText *" #> planElement.toString &
            "#segmentList *" #> (planElement match {
              case Activity(segments) => renderSegments(segments, distanceToNext)
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
    val clusters = Clusterer.findSignificantLocations(segments.asJava).asScala.toList
    clusters.map { cluster =>
      Facility("wurst", average(for (segment<-cluster.asScala.toList; location<-segment.segment.locations) yield location.location))
    }

    /*segments.map { segment =>
      Facility("wurst", segment.segment.locations.head.location)
    } */

  }

  def average(points : Seq[LatLong]): LatLong = {
    val n = points.size
    val sum = points.foldLeft((0.0,0.0)) { (p1,p2) => (p1._1+p2.lat, p1._2+p2.long) }
    LatLong(sum._1 / n, sum._2 / n)
  }


  def label(segments: List[Segment], facilities: List[Facility]) = {
    val labelledSegments = segments.map { segment =>
      def distanceToThisSegment(facility: Facility) = LatLong.calcDistance(facility.location, segment.locations.head.location)

      val needsFacility = segment.minutes >= 5 || segment.containsCheckin
      val nearestFacility = facilities match {
        case Nil => None
        case someFacilities => {
          val nearestFacility = someFacilities.reduceLeft((a,b) => if (distanceToThisSegment(a) <= distanceToThisSegment(b)) a else b)
          if (distanceToThisSegment(nearestFacility) <= SNAP_TO_FACILITY) Some(nearestFacility) else None
        }
      }
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

  def renderGoogleMap = renderLocations(actsAndLegs.toList)

  def renderLocations(planElements: List[PlanElement]): NodeSeq = {

    val activities = planElements.collect {case activity: Activity => activity}
    val activitiesAtFacilities = activities.groupBy(_._segments.head.facility.get)

    val jsLocations: Seq[JsObj] = activitiesAtFacilities.toList.map { entry =>
      val location = entry._1.location
      val times = entry._2.map(activity => new DateTime(activity.startTime).toString("HH:mm") + "-" + new DateTime(activity.endTime).toString("HH:mm"))
      JsObj(("title", times.toString),
      ("lat", location.lat.toString),
      ("lng", location.long.toString))
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
    val distance = LatLong.calcDistance(leg.activity1.head.segment.locations.head.location, leg.activity2.last.segment.locations.head.location)
    val duration = new Duration(new DateTime(leg.startTime), new DateTime(leg.endTime)).getStandardSeconds
    JsObj(("points", JsArray(List(leg.activity1.head.facility.get.location, leg.activity2.head.facility.get.location).map {
      location => JsObj(("lat", location.lat), ("lng", location.long))
    })), ("title",
      new DateTime(leg.startTime).toString("HH:mm") + "-" + new DateTime(leg.endTime).toString("HH:mm") + " "
        + (distance/1000).formatted("%.2f")+"km" + " "
    + ((distance / duration) * 3.6 formatted "%.2f"+"km/h")))
  }

  def segment(locations: List[Location]): Segmentation = {
    val locationAndNext = locations.zip(locations.tail)
    val locationAndDistance = locationAndNext.map( p => (p._1, LatLong.calcDistance(p._1.location,p._2.location)))
    val locationAndDistanceMap = Map() ++ locationAndDistance
    val segments = segmentLocations(locations).map(Segment.toSegment(_))
    Segmentation(segments, locationAndDistanceMap)
  }

  def segmentLocations(locations: List[Location]): List[List[Location]] = {
    locations match {
      case head :: tail => {
        val (cluster, rest) = (head :: tail).span(location => (LatLong.calcDistance(head.location, location.location) <= SMALL))
        cluster :: segmentLocations(rest)
      }
      case Nil => Nil
    }
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


  def toPlanElements(labelling: Labelling) = {
    val segments = labelling._1
    // group by switch of facility.
    // danach hab ich aber gemerkt, dass ich leere listen dazwischen haben will, wenn die facility von gesetzt auf
    // gesetzt wechselt .. total bloed.
    def groupByFacility(segments: List[LabelledSegment]) : List[List[LabelledSegment]] = {
      segments match {
        case Nil => Nil
        case firstSegment :: rest => {
          val (onSamePlanElement, onOtherPlanElements) = (firstSegment :: rest).span(segment => segment.facility == firstSegment.facility)
          onOtherPlanElements match {
            case next :: _ if firstSegment.facility != None && next.facility != None => onSamePlanElement :: Nil :: groupByFacility(onOtherPlanElements)
            case _ => onSamePlanElement :: groupByFacility(onOtherPlanElements)
          }

        }
      }
    }

    val groupedSegments = groupByFacility(segments)
    // hier die leeren listen wieder rausfischen. das muss besser gehn
    mySliding(groupedSegments).map { window => window match {
      case List(List(), List(dis), _) => (if (dis.head.facility == None) Other(dis) else Activity(dis)) : PlanElement
      case List(List(prev), List(dis), List(next)) => (if (dis.isEmpty || dis.head.facility == None) Leg(prev, dis, next) else Activity(dis)): PlanElement
      case List(List(prev), List(dis), List()) => (if (dis.head.facility == None) Other(dis) else Activity(dis)): PlanElement
    }


    }

  }

  def mySliding[T](lst: List[T]) = {
    (0 until lst.length).map(i => List(lst.slice(i - 1,i), List(lst(i)), lst.slice(i + 1, i + 2) ))
  }  // "This is a Zipper. Look at scalaz".



}
