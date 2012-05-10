package code
package snippet

import net.liftweb._
import scala.collection.JavaConversions._
import http._
import js._
import JsCmds._
import JE._
import comet.ChatServer
import data.mongo.Location
import org.bson.types.ObjectId
import data.mongo.LatLong
import code.oauth.LatWrapper
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
object Submit {

  def render = SHtml.onSubmit(s => {
        
    val latitude = new LatWrapper(LatitudeResource.is.openTheBox)
    val locations = latitude.getLatitude(SelectedDate.get)
    
    locations.foreach(jsonLoc => {
      val loc = Location(
          ObjectId.get, 
          LatLong(jsonLoc.getLatitude().asInstanceOf[java.math.BigDecimal].doubleValue(), jsonLoc.getLongitude().asInstanceOf[java.math.BigDecimal].doubleValue()), 
          new Date(jsonLoc.getTimestampMs().asInstanceOf[String].toLong))
      loc.save
    })
    
    
    
  })
  
}
