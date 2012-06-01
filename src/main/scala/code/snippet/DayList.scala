package code.snippet

import scala.collection.JavaConversions._
import _root_.net.liftweb.util._
import Helpers._
import code.oauth.LatWrapper
import net.liftweb.common.Logger
import data.mongo.Location
import java.text.SimpleDateFormat
import net.liftweb.http.SHtml

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
      val dateString = theDateFormat.format(date)
      "#maplink" #> ("a [href]" #> (dateString + "/map") & "a *" #> dateString) &
      "#listlink" #> ("a [href]" #> (dateString + "/"))
    })

  }

}
