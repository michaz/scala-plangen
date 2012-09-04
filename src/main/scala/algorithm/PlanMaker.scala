package algorithm

import algorithm.Labeller.{Segment, SegmentWithFacility}
import java.util.Date
import org.joda.time.{DateTime, Duration}

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 9/4/12
 * Time: 11:48 AM
 * To change this template use File | Settings | File Templates.
 */
object PlanMaker {






  abstract class PlanElement {
    def segments: List[Segment]
    def startTime: Date
    def endTime: Date
    def minutes = new Duration(new DateTime(startTime), new DateTime(endTime)).getStandardMinutes
  }

  case class Activity(_segments: List[SegmentWithFacility]) extends PlanElement {
    override def toString = "Act (" + startTime + "," + endTime + ") @ " + location
    override def startTime = _segments.head.segment.segment.startTime
    override def endTime = _segments.last.segment.segment.endTime
    def location = _segments.head.facility.get.location
    override def segments = _segments.map(_.segment.segment)
  }

  case class Leg(activity1: List[SegmentWithFacility], leg: List[SegmentWithFacility], activity2: List[SegmentWithFacility]) extends PlanElement {
    override def toString = "Leg (" + startTime + "," + endTime + "): (" + activity1.last.segment.segment.locations.last.location + "," + activity2.head.segment.segment.locations.head.location
    override def startTime = activity1.last.segment.segment.endTime
    override def endTime = activity2.head.segment.segment.startTime
    override def segments = leg.map(_.segment.segment)
  }

  case class Other(other: List[SegmentWithFacility]) extends PlanElement {
    override def toString = segments.size + " other Elements."
    override def segments = other.map(_.segment.segment)
    override def startTime = other.head.segment.segment.startTime
    override def endTime = other.last.segment.segment.endTime
  }



  object LegActivityTail {
    def unapply(xs: List[SegmentWithFacility]) = {
      val (leg, rest) = xs.span(_.facility.isEmpty)
      rest match {
        case act :: tail if act.facility.isDefined  => Some(leg,act,tail)
        case _ => None
      }
    }
  }


  def toPlanElements(segments: List[SegmentWithFacility]) = {
    // group by switch of facility.
    // danach hab ich aber gemerkt, dass ich leere listen dazwischen haben will, wenn die facility von gesetzt auf
    // gesetzt wechselt .. total bloed.
    def groupByFacility(segments: List[SegmentWithFacility]) : List[List[SegmentWithFacility]] = {
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
