package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.Logger
import data.mongo.Location
import java.text.SimpleDateFormat
import algorithm.Truth

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
object DayList extends Logger {

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  def render = "li *" #> {
    Location.findDays.map(date => {
      val truth = new Truth(date.toDate)
      truth.load
      val dateString = theDateFormat.format(date.toDate)
      "#maplink" #> ("a [href]" #> (dateString + "/map") & "a *" #> dateString) &
      "#listlink" #> ("a [href]" #> (dateString + "/")) &
      "#planlink" #> ("a [href]" #> (dateString + "/plan")) &
      "#hastruth *" #> (if (truth.contents.isEmpty) "" else "truth")
    })

  }

}
