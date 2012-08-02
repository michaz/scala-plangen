package code.snippet

import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.common.Full
import net.liftweb.common.Logger
import data.mongo.Location
import xml.Text
import java.util.Date
import net.liftweb.http.S
import java.text.SimpleDateFormat

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
object LatitudeList extends Logger {

  private[this] def theDateFormat = new SimpleDateFormat("yyyy-MM-dd")

  def render = S.param("date") match {
    case Full(dateParam) => {
      val date = theDateFormat.parse(dateParam)
      val locations = Location.findByDay(date)
      "li *" #> {
        (locations sortBy (_.timestamp.getTime)).map(_.toString)
      } &
        "#num *" #> Text(locations.size.toString) &
        "#from_date *" #> Text(date.toString) &
        "#to_date *" #> Text(new Date(date.getTime + 24 * 60 * 60 * 1000).toString)
    }
    case _ => "li *" #> "No date."
  }
}
