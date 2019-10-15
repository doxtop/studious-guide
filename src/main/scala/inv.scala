package t4s
package inventory

import java.time.LocalDate
import cats._, data._, implicits._
import io.circe.{Encoder, KeyEncoder}

/** 
 * Genres of available shows encoded here. 
 */
sealed trait Genre
case object Musical extends Genre
case object Drama   extends Genre
case object Comedy  extends Genre

object Genre {
    implicit val showGenre:Show[Genre] = Show.show( {case Musical => "musical"; case Drama => "drama"; case Comedy => "comedy"} )
    implicit val readGenre:Read[Genre] = Read.read( _ match {
        case "musical"  => Musical.some
        case "drama"    => Drama.some
        case "comedy"   => Comedy.some
        case _          => None
    })

    implicit val genreEncpder:Encoder[Genre]        = Encoder.encodeString.contramap[Genre](_.show)
    implicit val genreKeyEncoder: KeyEncoder[Genre] = KeyEncoder.instance( _.show )
}

/** 
 * Sales: status.
 */
sealed trait Sale
case object NotStarted  extends Sale
case object OpenForSale extends Sale
case object SoldOut     extends Sale
case object InThePast   extends Sale

object Sale {
    implicit val showStatus:Show[Sale] = Show.show({
        case InThePast   => "in the past";
        case NotStarted  => "not started";
        case OpenForSale => "open for sale";
        case SoldOut     => "sold out" })

    implicit val eqStatus:Eq[Sale] = Eq.allEqual

    implicit val satusEncoder:Encoder[Sale] = Encoder.encodeString.contramap[Sale](_.show)
}

/** Hall type */
sealed abstract class Hall(val seats:Int)
case class Big() extends Hall(200)
case class Sml() extends Hall(100)


/** 
 * Particular show performance for wich we are able to calculate tickets information.
 */
case class Performance(genre:Genre, title:String, status:Sale, air:LocalDate, vacant:Int, available:Int, hall:Hall, price: Int)

object Performance {
    implicit val encodePerformance: Encoder[Performance] = 
        Encoder.forProduct5("title", "tickets left", "tickets available" ,"status", "price")(p => 
            (p.title, p.vacant, p.available, p.status, p.price))
}
