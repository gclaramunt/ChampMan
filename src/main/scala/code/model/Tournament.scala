package code.model

import net.liftweb.mapper._
import java.text.DateFormat
import xml.Text

/**
 * Created by IntelliJ IDEA.
 * User: gabriel.claramunt
 * Date: Oct 22, 2010
 * Time: 3:33:31 PM
 * To change this template use File | Settings | File Templates.
 */

class Tournament extends LongKeyedMapper[Tournament] with IdPK  {
  def getSingleton = Tournament

   // Which user created this tournament
  object owner extends MappedLongForeignKey(this, User) {
    override def dbIndexed_? = true
  }

  // Which users, besides the owner, are allowed to modify this tournament
  //def admins = TournamentAdmin.findAll(By(TournamentAdmin.tournament, this.id))


  object name extends MappedString(this,100)

  object description extends MappedString(this, 300)

  // Optional notes about the tournament
  def notes = TournamentNote.findAll(By(TournamentNote.tournament, this.id))

  object beginDate extends MappedDateTime(this) {
    final val dateFormat =
      DateFormat.getDateInstance(DateFormat.SHORT)
    override def asHtml = Text(dateFormat.format(is))
  }

  object endDate extends MappedDateTime(this) {
    final val dateFormat =
      DateFormat.getDateInstance(DateFormat.SHORT)
    override def asHtml = Text(dateFormat.format(is))
  }
}

object Tournament extends Tournament with LongKeyedMetaMapper[Tournament] {
  def findByName (owner : User, name : String) : List[Tournament] =
    Tournament.findAll(By(Tournament.owner, owner.id.is), By(Tournament.name, name))

  import net.liftweb.util.Helpers.tryo

  /**
   * Define an extractor that can be used to locate a Tournament based
   * on its ID.
   */
  def unapply (id : String) : Option[Tournament] = tryo {
    find(By(Tournament.id, id.toLong)).toOption
  } openOr None
}


class TournamentAdmin extends LongKeyedMapper[TournamentAdmin] with IdPK {
  def getSingleton = TournamentAdmin

  object tournament extends MappedLongForeignKey(this, Tournament) {
    override def dbIndexed_? = true
  }

  object administrator extends MappedLongForeignKey(this, User) {
    override def dbIndexed_? = true
  }
}
object TournamentAdmin extends TournamentAdmin with LongKeyedMetaMapper[TournamentAdmin]


class TournamentNote extends LongKeyedMapper[TournamentNote] with IdPK {
  def getSingleton = TournamentNote

  object tournament extends MappedLongForeignKey(this, Tournament) {
    override def dbIndexed_? = true
  }

  object note extends MappedText(this)
}

object TournamentNote extends TournamentNote with LongKeyedMetaMapper[TournamentNote] {
  override def fieldOrder = note :: Nil
}