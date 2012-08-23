package algorithm

import data.mongo.{LatLong, Location}
import org.joda.time.{DateTime, Duration}
import net.liftweb.json.JsonAST.JNothing
import util.Clusterer.ThingToCluster
import util.Clusterer
import scala.collection.JavaConverters._
import service.User

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 8/13/12
 * Time: 3:22 PM
 * To change this template use File | Settings | File Templates.
 */


object Labeller {

  val DURATION_OF_SIGNIFICANT_ACTIVITY = 5.0
  val SEGMENT_SIZE = 50.0
  val SNAP_TO_FACILITY = 200.0

  type Labelling = (List[LabelledSegment], List[LabelledFacility])

  case class LabelledSegment(segment: Segment, needsFacility: Boolean, facility: Option[Facility]) extends ThingToCluster {
    def getLat = segment.locations.head.location.lat
    def getLong = segment.locations.head.location.long
  }

  case class Facility(name: String, location: LatLong)

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

  case class LabelledFacility(facility: Facility)

  case class Segmentation(segments: List[Segment], distanceToNext: Map[Location, Double])

  def labelLocations(locations: List[Location]) = {
    val Segmentation(segments, distanceToNext) = segment(locations)
    var facilities: List[Facility] = Nil
    var labelling = label(segments, facilities)

    for (i <- 1 to 1) {
      facilities = deriveFacilities(labelling._1.filter(_.needsFacility))
      labelling = label(segments, facilities)
    }

    val finalLabelling = labelling
    (finalLabelling, distanceToNext)
  }

  def labelLocationsWithBackground(user: User, locations: List[Location]) = {
    val backgroundFacilities = computeBackgroundFacilities(user)
    val Segmentation(segments, distanceToNext) = segment(locations)
    val labelling = labelWithBackground(segments, backgroundFacilities)
    (labelling, distanceToNext)
  }

  def computeBackgroundFacilities(user: User) = {
    val allLocations = user.findAllLocations
    val Segmentation(segments, distanceToNext) = segment(allLocations)
    var facilities: List[Facility] = Nil
    var labelling = label(segments, facilities)
    for (i <- 1 to 1) {
      facilities = deriveFacilities(labelling._1.filter(_.needsFacility))
      labelling = label(segments, facilities)
    }
    labelling._2
  }


  def label(segments: List[Segment], facilities: List[Facility]): (List[LabelledSegment], List[LabelledFacility]) = {
    val labelledSegments = segments.map { segment =>
      def distanceToThisSegment(facility: Facility) = LatLong.calcDistance(facility.location, segment.locations.head.location)

      val needsFacility = segment.minutes >= DURATION_OF_SIGNIFICANT_ACTIVITY || segment.containsCheckin
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

  def labelWithBackground(segments: List[Segment], backgroundFacilities: List[LabelledFacility]) = {
    label(segments, backgroundFacilities.map(_.facility))
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
        val (cluster, rest) = (head :: tail).span(location => (LatLong.calcDistance(head.location, location.location) <= SEGMENT_SIZE))
        cluster :: segmentLocations(rest)
      }
      case Nil => Nil
    }
  }

  def deriveFacilities(segments: List[LabelledSegment]) = {
    val clusters = Clusterer.findSignificantLocations(segments.asJava).asScala.toList
    clusters.map { cluster =>
      Facility("wurst", average(for (segment<-cluster.asScala.toList; location<-segment.segment.locations) yield location.location))
    }
  }

  def average(points : Seq[LatLong]): LatLong = {
    val n = points.size
    val sum = points.foldLeft((0.0,0.0)) { (p1,p2) => (p1._1+p2.lat, p1._2+p2.long) }
    LatLong(sum._1 / n, sum._2 / n)
  }

}
