package t4s

import scala.io._
import java.time._, format._
import cats._, data._, implicits._, effect._

import org.http4s._, org.http4s.dsl.io._
import org.http4s.server.blaze._

import com.typesafe.config.ConfigFactory

import org.scalatest._, prop.Checkers
import org.scalacheck._, Prop._, Arbitrary._

import inventory._
import store._
import decode._
import routes._, inv._
import Core._

class RouteSpec extends FunSpec with Checkers with GenInventory with DecodeInventory {
    import org.http4s.implicits._

    override def withFixture(test:NoArgTest) = {test()}

    val cfg = ConfigFactory.load
    val today = LocalDate.now
    implicit val reg:Regulation = new Reg(cfg.getConfig("regulation.limits"))
    implicit val dba:Dba = Store

    def testDb = fromResource("shows.csv").done.option
        .foldMap(_.map(r => scheduleDay(today,today,CvsRow.unapply(r).get)))
        .map((today, _))
        .groupBy(_._2.title)

    dba.init(testDb)

    it("should return inventory on specific date") {

        val getDate = Request[IO](Method.GET, uri("inventory/date/2018-02-26"))
        val io = datei.orNotFound.run(getDate)

        val x = io.unsafeRunSync
        
        assert(x.status == Ok)
        // body asserts here
    }

    ignore("dev server") {
        lazy val routes = static <+> datei <+> genrei <+> orderi

        val builder = BlazeBuilder[IO].bindHttp(8080, "localhost")
            .mountService(routes, "/")
            .start
        val server = builder.unsafeRunSync

        println("http://localhost:8080/index.html")

        scala.io.StdIn.readLine
        server.shutdown.unsafeRunSync
    }

    it("pr"){
        val m:Map[String, Row] = testDb
        info(s"""${fromResource("shows.csv").done.either}""")
        //info(s"${m.values.size}")
    }
}
