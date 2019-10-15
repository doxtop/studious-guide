package t4s

import com.typesafe.config.ConfigFactory
import java.time.LocalDate
import fs2.{Stream, StreamApp}
import fs2.StreamApp.ExitCode
import cats._, data._, implicits._, effect._
import org.http4s._, dsl.io._, server.blaze._

import routes._, inv._
import store._
import decode._
import Core._

/**
 * T4S web entry point.
 */
object WebApp extends StreamApp[IO] with DecodeInventory {
    
    // configure
    val cfg  = ConfigFactory.load
    val port = cfg.getInt("http.port")
    val host = cfg.getString("http.host")
    val today = LocalDate.now

    implicit val dba:Dba = Store
    implicit val reg:Regulation = new Reg(cfg.getConfig("regulation.limits"))
    
    // init db
    dba.init(
        fromResource("shows.csv").done
        .option
        .foldMap(_.map(r => scheduleDay(today,today,CvsRow.unapply(r).get)))
        .map((today, _))
        .groupBy(_._2.title)
    )

    // serve
    override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = 
        BlazeBuilder[IO]
        .bindHttp(port, host)
        .mountService(static <+> datei <+> genrei <+> orderi, "/")
        .serve
}
