package code.snippet
import java.text.SimpleDateFormat
import scala.xml.NodeSeq
import scala.xml.Null
import scala.xml.UnprefixedAttribute
import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.OnLoad
import net.liftweb.http.js.JsCmds.Script
import net.liftweb.http.js.JsCmds.jsExpToJsCmd
import net.liftweb.http.LiftScreen
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers
import net.liftweb.http.SessionVar
import net.liftweb.common.Empty

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

object SelectedDate extends SessionVar[Box[Long]](Empty)

object DatePicker extends LiftScreen {

	val date = new Field {
		private[this] def theDateFormat = new SimpleDateFormat("MM/dd/yyyy")
		type ValueType = Long
		override def name = "Start Date " 
		override implicit def manifest = buildIt[Long] 
		override def default = 47l 
		override def toForm: Box[NodeSeq] = {
			import net.liftweb.util._


			def getStringValue = theDateFormat.format(is)

			def setDate(in: String): Unit = { set( theDateFormat.parse(in).getTime) }

			import net.liftweb._
			import http.js._
			import JsCmds._
			import JE._

			val cssClass = Helpers.nextFuncName

			val elem = SHtml.text(getStringValue, setDate _) 

			Full(elem) 
//			    ++
//					Script(
//							OnLoad(
//									JsRaw("try{jQuery('."+cssClass+"').datepicker({changeYear: true, changeMonth: true, yearRange: '-50:+50', maxDate: '+2y', minDate: '-100y'});} catch(e) {alert(e);}"))))
		}
	}
	
	def finish() {
		S.notice("Date: "+date)
		SelectedDate.set(Full(date))
	}

}
