package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.{Full, Logger}
import java.text.SimpleDateFormat
import xml.{NodeSeq, Text}
import net.liftweb.http._
import data.mongo.{TruthRecord, TruthItem, LatLong, Location}
import org.joda.time.{Interval, DateTime, Duration}
import net.liftweb.http.js.{JsExp, JE, JsCmd, JsObj}
import net.liftweb.http.js.JE.{JsRaw, JsArray, JsObj}
import net.liftweb.http.js.JsCmds.{OnLoad, Script, JsCrVar}
import java.util.{Locale, Date}
import algorithm.Labeller._
import algorithm.Labeller
import bootstrap.liftweb.CurrentUser
import net.liftweb.http.js.jquery.JqWiringSupport
import net.liftweb.util
import scala.Some
import algorithm.Labeller.LabelledSegment
import xml.Text
import net.liftweb.common.Full
import org.joda.time.format.DateTimeFormat

import org.bson.types.ObjectId

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

  class Truth {

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
        for (truthRecord <- TruthRecord.findByDay(date)) {
          temp = temp :+ truthRecord.ti
        }
        temp
      })
    }

    def save {
      var oldTruth = TruthRecord.findByDay(date)
      for (truthItem <- oldTruth) {
        truthItem.delete
      }
      for (truthItem <- contents) {
        TruthRecord(ObjectId.get, CurrentUser.is.openTheBox.currentUserId, truthItem).save
      }
    }

  }


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

  private[this] val theDateFormat = new SimpleDateFormat("yyyy-MM-dd")
  private[this] val theTimeFormat = DateTimeFormat.mediumTime().withLocale(Locale.GERMANY)

  val theTruth = new Truth() // Previously resolved a RequestVar. If I access the RequestVar directly from the AJAX callbacks,
  // they are not restored. Perhaps Lift bug #980 ? But this way I think it is a correct workaround - all the callbacks
  // close around the same object. Perhaps I wouldn't even need a RequestVar.

  var actsAndLegs: Seq[PlanElement] = Nil

  var facilities: Seq[Facility] = Nil

  var date: Date = new Date()

  def render = {

    def renderSegments(segments: scala.List[SegmentWithFacility], distanceToNext: Map[Location, Double]): List[CssSel] = {
      segments.map { segment =>
        "#locationList *" #> segment.segment.segment.locations.map { location =>
          "#locationText *" #> Text(location.toString) &
          "#locationDist *" #> distanceToNext.get(location).getOrElse(0.0).toString
        } & "#segmentText *" #> Text(segment.segment.segment.minutes + " Minuten langes Segment. Signifikant: " + segment.segment.isActivity)

      }
    }

    S.param("date") match {
      case Full(dateParam) => {
        date = theDateFormat.parse(dateParam)
        theTruth.load
        val locations = Location.findByDay(date)
        val user = CurrentUser.is.openTheBox
        val backgroundFacilities = computeBackgroundFacilities(user) // later: LOADbackgroundfacilities (and trained network)

        val Segmentation(segments, distanceToNext) = segment(locations)

        val finalLabelling = labelWithBackground(segments, backgroundFacilities)
        val augmentedLabelling = augmentWithTruth(finalLabelling._1) //perhaps: put truth in already?


        // not using labelled facilities from labelling, but inferring facilities freshly from only today (with truth)
        val finalFacilities = Labeller.deriveFacilities(augmentedLabelling.filter(s => s.isActivity))
        val withNearestFacility = snapActivitiesToNearestFacility(augmentedLabelling, finalFacilities)

        actsAndLegs = toPlanElements(withNearestFacility)
        facilities = finalFacilities
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

  def snapActivitiesToNearestFacility(segments: List[LabelledSegment], facilities: List[Facility]) = {
    for (segment <- segments) yield {
      if (segment.isActivity)
        SegmentWithFacility(segment, Labeller.findNearFacility(segment.segment, facilities))
      else
        SegmentWithFacility(segment, None)
    }
  }

  def augmentWithTruth(finalLabelling: List[LabelledSegment]): List[LabelledSegment] = {
    for (segment <- finalLabelling) yield {
      val inTrueActivity = theTruth.contents.filter(_.tag == "act") exists { t =>
        new Interval(new DateTime(segment.segment.startTime), new DateTime(segment.segment.endTime))
        .overlaps(new Interval(new DateTime(t.from), new DateTime(t.to)))
      }
      if (inTrueActivity) segment.copy(isActivity = inTrueActivity) else segment
    }
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

  def renderGoogleMap = renderLocations(actsAndLegs.toList, facilities.toList)

  def renderLocations(planElements: List[PlanElement], facilities: List[Facility]): NodeSeq = {

    val activities = planElements.collect {case activity: Activity => activity}
    val activitiesAtFacilities = activities.groupBy(_._segments.head.facility.get)

    val jsLocations: Seq[JsObj] = activitiesAtFacilities.toList.map { entry =>
      val location = entry._1.location
      val times = entry._2.map(activity => new DateTime(activity.startTime).toString("HH:mm") + "-" + new DateTime(activity.endTime).toString("HH:mm"))
      JsObj(("title", times.toString),
      ("lat", location.lat.toString),
      ("lng", location.long.toString))
    } ::: (for (facility <- facilities if !activitiesAtFacilities.contains(facility) ) yield {
      JsObj(("title", "non-used facility"),
        ("lat", facility.location.lat.toString),
        ("lng", facility.location.long.toString))
    })

    val jsLegs: Seq[JsObj] = planElements.collect {
      case leg: Leg => makeLeg(leg)
    }

    (<head>
      {Script(OnLoad(ajaxFuncDrawMap(jsLocations, jsLegs)))}
      {Script(OnLoad(ajaxFuncDrawChart(planElements)))}
    </head>)
  }

  def renderTruth = {
    "tbody" #> (

      util.Helpers.findOrCreateId(id =>  // make sure tbody has an id
      // when the cart contents updates
        WiringUI.history(theTruth.contents) {
          (old, nw, ns) => {
            // capture the tr part of the template
            val theTR = ("tr ^^" #> "**")(ns)
            def ciToId(ci: TruthItem): String = ci.id

            // build a row out of a cart item
            def html(ci: TruthItem): NodeSeq = {
              ("tr [id]" #> ciToId(ci) &
                "@from *" #> Text(theTimeFormat.print(new DateTime(ci.from))) &
                "@to *" #> Text(theTimeFormat.print(new DateTime(ci.to))) &
                "@tag *" #> Text(ci.tag) &
                "@del [onclick]" #> SHtml.ajaxInvoke(() => {
                  theTruth.removeItem(ci)
                }) &
                "@merge [onClick]"#> SHtml.ajaxInvoke(() => {
                  if (theTruth.isMergeable(ci)) {
                    theTruth.merge(ci)
                  }
                })
                )(theTR)
            }

            // calculate the delta between the lists and
            // based on the deltas, emit the current jQuery
            // stuff to update the display
            JqWiringSupport.calculateDeltas(old, nw, id)(ciToId _, html _)
          }
        })
      )
  }



  def renderTruthButton = {
    var from = theTimeFormat.print(new DateTime())
    var to = theTimeFormat.print(new DateTime())
    var tag = "c"
      // "@from" #> SHtml.text(theDateFormat.format(date), content => JsCmds.RedirectTo(MyBoot.googleUrl(("/google_map/" + content))))
    "@from *" #> SHtml.text(from, content => from = content) &
    "@to *" #> SHtml.text(to, content => to = content)  &
    "@tag *" #> SHtml.text(tag, content => {
      tag = content
      theTruth.addItem(TruthItem(new DateTime(date).withFields(theTimeFormat.parseLocalTime(from)).toDate, new DateTime(date).withFields(theTimeFormat.parseLocalTime(to)).toDate, tag))
    }) &
    "@fill [onClick]" #> SHtml.ajaxInvoke(() => {fillTruth})
  }

  def fillTruth = {
    theTruth.clear
    for (planElement <- actsAndLegs) {
      planElement match {
        case a:Activity => {
          theTruth.addItem(TruthItem(a.startTime, a.endTime, "act"))
        }
        case l:Leg => {
          theTruth.addItem(TruthItem(l.startTime, l.endTime, "leg"))
        }
        case _ => {}
      }
      }
    }



  def makeLeg(leg: ShowPlan.this.type#Leg): JsObj = {
    val distance = LatLong.calcDistance(leg.activity1.head.segment.segment.locations.head.location, leg.activity2.last.segment.segment.locations.head.location)
    val duration = new Duration(new DateTime(leg.startTime), new DateTime(leg.endTime)).getStandardSeconds
    JsObj(("points", JsArray(List(leg.activity1.head.facility.get.location, leg.activity2.head.facility.get.location).map {
      location => JsObj(("lat", location.lat), ("lng", location.long))
    })), ("title",
      new DateTime(leg.startTime).toString("HH:mm") + "-" + new DateTime(leg.endTime).toString("HH:mm") + " "
        + (distance/1000).formatted("%.2f")+"km" + " "
    + ((distance / duration) * 3.6 formatted "%.2f"+"km/h")))
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
