package t4s
package routes

import java.time.LocalDate
import cats.effect._
import org.http4s._, org.http4s.dsl.io._

import store._ 

trait InventoryRoute {

    def datei(implicit dba:Dba, reg:Regulation) = HttpService[IO] {
        case GET -> Root / "inventory" / "date" / LocalDateVar(date) => encodeRes(dba.loadDate(date))
    }
    
    def genrei(implicit dba:Dba, reg:Regulation) = HttpService[IO] {
        // genre filter on UI for the moment
        case GET -> Root / "inventory" / "genre" / genre => encodeRes(dba.loadDate(LocalDate.now))
    }

}