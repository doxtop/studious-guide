package t4s
package routes

import cats._, data._, implicits._, effect._
import org.http4s._, org.http4s.dsl.io._

import store._

trait TicketsRoute {

    /**
     * Handle tickets ordering for specified show on specified date.
     */
    def orderi(implicit dba:Dba, reg:Regulation) = HttpService[IO] {
        case GET -> Root / "order" / title / LocalDateVar(date) =>
            encodeRes((for {
                show    <- EitherT.fromEither[Id](dba.get(title))
                dated   <- EitherT.fromOption[Id](show.filter(_._1 == date).headOption, ShowNotOnAir(s"$show") : Err)
                limit   <- EitherT.fromEither[Id](reg.dayLimit(dated._2.hall))
                update  <- EitherT.cond[Id](
                    dated._2.vacant <= limit && dated._2.vacant > 0,
                    dated._2.copy(vacant = dated._2.vacant - 1, available = dated._2.available - 1),
                    SaleLimit(dated._2.title) : Err)
                _       <- EitherT.fromEither[Id](dba.put(title, show.filter(_._1 != date) |+| List((date, update))) )
                db      <- EitherT.fromEither[Id](dba.loadDate(date))
            } yield db).value)
            
    }
}
