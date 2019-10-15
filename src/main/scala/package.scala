import cats._, data._, implicits._
import java.time._, format._
import scala.collection._

/** Application wise types */

package object t4s {

    implicit val dateOrder:Order[LocalDate] = Order.fromOrdering[LocalDate](Ordering.by(_.toEpochDay))

    /** Read till strings are coming (json, cvs, etc). */

    trait Read[A] { def read(a: String): Option[A] }

    object Read {
        def read[A] (f: String => Option[A]): Read[A] = new Read[A] {
            def read(a: String): Option[A] = f(a)
        }
        def apply[A: Read]: Read[A] = implicitly[Read[A]]
    }

    /** Application errors */

    sealed trait Err
    case class  ShowNotFound(title:String) extends Err
    case class  ShowNotOnAir(date:String) extends Err
    case class  SaleLimit(title:String) extends Err
    case object DayScheduleExist extends Err
    case class  GenErr(msg:String) extends Err

    object Err {
        implicit val showErr:Show[Err] = Show.show({
            case ShowNotFound(t)    => s"$t not found"
            case ShowNotOnAir(d)    => s"show will not be performed at $d"
            case SaleLimit(_)       => s"No more tickets available"
            case DayScheduleExist   => s"This day mapping already calculated"
            case _:Err              => "unknown"})

        implicit val readErr:Read[Err] = Read.read({_ => GenErr("some error").some} )

        import io.circe.Encoder
        implicit val errEncoder:Encoder[Err] = Encoder.encodeString.contramap[Err](_.show)
    }
}
