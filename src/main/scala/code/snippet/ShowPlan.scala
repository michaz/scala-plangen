package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.Logger
import java.text.SimpleDateFormat
import xml.NodeSeq
import net.liftweb.http._
import data.mongo.{LatLong, Location}
import org.joda.time._
import net.liftweb.http.js.{JsExp, JsCmd, JsObj}
import net.liftweb.http.js.JE.{JsRaw, JsArray, JsObj}
import net.liftweb.http.js.JsCmds.{OnLoad, Script, JsCrVar}
import java.util.{Locale, Date}
import algorithm.Labeller._
import algorithm._
import bootstrap.liftweb.CurrentUser
import net.liftweb.http.js.jquery.JqWiringSupport
import org.joda.time.format.DateTimeFormat

import net.liftweb.util
import algorithm.Labeller.SegmentWithFacility
import scala.Some
import algorithm.Labeller.LabelledSegment
import algorithm.Labeller.Facility
import xml.Text
import data.mongo.TruthItem
import algorithm.Labeller.Segmentation
import net.liftweb.common.Full
import algorithm.PlanMaker.{Other, Leg, Activity, PlanElement}

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


  // val ALGORITHM="truth"
  val ALGORITHM="naive"

  // val ALGORITHM="learning"

  private[this] val theDateFormat = new SimpleDateFormat("yyyy-MM-dd")
  private[this] val theTimeFormat = DateTimeFormat.mediumTime().withLocale(Locale.GERMANY)

   // they are not restored. Perhaps Lift bug #980 ? But this way I think it is a correct workaround - all the callbacks
  // close around the same object. Perhaps I wouldn't even need a RequestVar.

  var actsAndLegs: Seq[PlanElement] = Nil

  var facilities: Seq[Facility] = Nil

  var date: Date = new Date()

  var theTruth: Truth = null // Previously resolved a RequestVar. If I access the RequestVar directly from the AJAX callbacks,

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
        theTruth = new Truth(date)
        theTruth.load
        val locations = Location.findByDay(new LocalDate(date))
        val user = CurrentUser.is.openTheBox

        val Segmentation(segments, distanceToNext) = segment(locations)
        val labelling = if (ALGORITHM=="learning") {
          val labelling = Evaluate.evaluate(segments)
          labelling.toList
        } else if (ALGORITHM=="naive") {
          val backgroundFacilities = computeBackgroundFacilities(user) // later: LOADbackgroundfacilities (and trained network)
          val finalLabelling = labelWithBackground(segments, backgroundFacilities)
          finalLabelling._1
        } else {
          Truth.labelWithTruth(segments, theTruth.contents)
        }

        // not using labelled facilities from labelling, but inferring facilities freshly from only today
        val finalFacilities = Labeller.deriveFacilities(labelling.filter(s => s.isActivity))
        val withNearestFacility = snapActivitiesToNearestFacility(labelling, finalFacilities).toList

        val planElements = PlanMaker.toPlanElements(withNearestFacility)


        actsAndLegs = planElements
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
        } &
        "#plan *" #> (
          "tr *" #> {
            actsAndLegs.map { planElement =>
              "@from *" #> Text(theTimeFormat.print(new LocalDateTime(planElement.startTime))) &
                "@to *" #> Text(theTimeFormat.print(new LocalDateTime(planElement.endTime))) &
                "@tag *" #> Text(planElement match {
                  case _:Activity => "act "
                  case _:Leg => "leg"
                  case _ => "other"
                })
            }
          }
          )
      }
      case _ => {
        "#plan *" #> Text("Not logged in.")
      }
    }

  }


  def augmentWithTruth(finalLabelling: List[LabelledSegment]): List[LabelledSegment] = {
    val (truth, _) = theTruth.contents.currentValue
    Labeller.augmentWithTruth(finalLabelling, truth)
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



  def makeLeg(leg: Leg): JsObj = {
    val distance = LatLong.calcDistance(leg.activity1.head.segment.segment.locations.head.location, leg.activity2.last.segment.segment.locations.head.location)
    val duration = new Duration(new DateTime(leg.startTime), new DateTime(leg.endTime)).getStandardSeconds
    JsObj(("points", JsArray(List(leg.activity1.head.facility.get.location, leg.activity2.head.facility.get.location).map {
      location => JsObj(("lat", location.lat), ("lng", location.long))
    })), ("title",
      new DateTime(leg.startTime).toString("HH:mm") + "-" + new DateTime(leg.endTime).toString("HH:mm") + " "
        + (distance/1000).formatted("%.2f")+"km" + " "
    + ((distance / duration) * 3.6 formatted "%.2f"+"km/h")))
  }




}
