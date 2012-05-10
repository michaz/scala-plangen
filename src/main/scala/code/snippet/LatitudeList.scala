package code.snippet
import scala.collection.JavaConversions._
import _root_.net.liftweb.util._
import Helpers._
import code.oauth.LatWrapper
import net.liftweb.common.Full
import net.liftweb.common.Logger
import data.mongo.Location
import org.bson.types.ObjectId
import data.mongo.LatLong

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

  def render = "li *" #> {
    warn("I'm in the snippet")
    val latitude = new LatWrapper(LatitudeResource.is.openTheBox)
    val locations = latitude.getLatitude(SelectedDate.get)

    (locations sortBy ( _.getTimestampMs().asInstanceOf[String] )) .map(_.toString)
  }

}
