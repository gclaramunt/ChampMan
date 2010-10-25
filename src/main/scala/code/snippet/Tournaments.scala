package code.snippet

import xml.{Text, NodeSeq}
import net.liftweb.common.{Box,Empty,Full,Logger}
import net.liftweb.http.{SHtml, FileParamHolder, S, StatefulSnippet}

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import code.lib._
import Helpers._
import code.util.Util
import code.model.{Tournament, User}

/**
 * Created by IntelliJ IDEA.
 * User: gabriel.claramunt
 * Date: Oct 22, 2010
 * Time: 3:49:55 PM
 * To change this template use File | Settings | File Templates.
 */

class Tournaments {
  def summary (xhtml : NodeSeq) : NodeSeq = User.currentUser match {
    case Full(user) => {
      val entries : NodeSeq = user.ownedTournaments match {
	      case Nil => Text("You have no tournaments set up") // TODO: Add link to create one...
	      case tournaments => tournaments.flatMap({tournament =>
	          bind("t", chooseTemplate("tournament", "entry", xhtml),
	          "name" -> <a href={"/tournament/" + tournament.name.is}>{tournament.name.is}</a>,
            "bgDate" -> tournament.beginDate,
            "endDate" -> tournament.beginDate)
				})
      }
      bind("tournament", xhtml, "entry" -> entries)
    }
    case _ => <lift:embed what="welcome_msg" />
  }

}

class AddEntry extends StatefulSnippet {
  def dispatch : DispatchIt = {
    case "addentry" => add _
  }

  /*
  name
  description
  notes
  beginDate
  endDate
   */

  var name  = ""
  var description = ""
  var beginDate = ""
  var endDate = ""

  def add(in: NodeSeq): NodeSeq = User.currentUser match {
    case Full(user)  => {

      def doTagsAndSubmit(t: String) {
          val fromDate = Util.slashDate.parse(beginDate)
          val toDate = Util.slashDate.parse(endDate)

          val tournament = Tournament.create.name(name).description(description).beginDate(fromDate).endDate(toDate)
          (tournament.validate) match {
            case Nil => tournament.save
                        println ("::::::::::::::Saved tournament "+tournament)
                        this.unregisterThisSnippet() // dpp: remove the statefullness of this snippet
            case x => S.error(x)
          }
      }

      bind("e", in,
           "name" -> SHtml.text("", name = _),
           "description" -> SHtml.text("", description = _),
           "beginDate" -%> SHtml.text("", beginDate = _) % ("size" -> "10"),
           "endDate" -%> SHtml.text("", endDate = _) % ("size" -> "10")
           )
    }
    case _ => Text("")
  }
}