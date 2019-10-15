package t4s
package decode

import com.typesafe.config.ConfigFactory
import java.time._, temporal._, format._
import cats._, data._, implicits._
import org.scalatest._, prop.Checkers
import org.scalacheck._, Prop._, Arbitrary._

import org.scalacheck.Test.Parameters

import inventory._
import Core._

/** 
 * Max discard ratio is manually configured for each test to save the generators time while keep them useful.
 */
class CoreSpec extends FunSpec with Checkers with GenInventory {

    implicit val reg:Regulation = new Reg(ConfigFactory.load.getConfig("regulation.limits"))
    
    describe("A show") {

        def inPast(s:LocalDate, q:LocalDate, a:LocalDate) = s < q || q > a.plusDays(100) || s > a.plusDays(100)

        it("Shows always run for exactly 100 days in the theatre") {
            check(forAll(genShow, arbitrary[LocalDate], arbitrary[LocalDate]) { (i:CvsRow, q:LocalDate, s:LocalDate) =>
                val p = scheduleDay(q,s,CvsRow.unapply(i).get)
                val past = inPast(s, q, i.date)

                ("evidence:" + p) |: all(
                    "status"            |: past ==> (p.status    ?= InThePast),
                    "available today"   |: past ==> (p.vacant    ?= 0),
                    "total tickets"     |: past ==> (p.available ?= 0)
                )
            })
        }

        it("Ticket sale should't start earlier when 25 days before a show starts") {
            check (
                forAll (genShow, arbitrary[LocalDate], arbitrary[LocalDate]) { (i:CvsRow, q:LocalDate, s:LocalDate) =>
                    val p = scheduleDay(q,s,CvsRow.unapply(i).get)
                    val past = inPast(s, q, i.date)
                    val notStarted = s > q.plusDays(25)

                    ("evidence:" + p) |: all(
                        "status"          |: (!past && notStarted) ==>  (p.status       ?= NotStarted),
                        "available today" |: (!past && notStarted) ==>  (p.vacant       ?= 0),
                        "total tickets"   |: (!past && notStarted) ==>  (p.available    ?= p.hall.seats)
                    )
                },
                Parameters.default.withMaxDiscardRatio(20))
        }

        it("Ticket sale should start 25 days before a show starts") {
            check(forAll(genShow, arbitrary[LocalDate], arbitrary[LocalDate]){ (i:CvsRow, q:LocalDate, s:LocalDate) =>
                val p = scheduleDay(q,s,CvsRow.unapply(i).get)
                val past = inPast(s, q, i.date)
                val openForSale = q >= s.minusDays(25) && q < s.minusDays(5)

                val daysOnSale = s.minusDays(25).until(q, ChronoUnit.DAYS).toInt
                val daylySale  = reg.dayLimit(p.hall).getOrElse(-1)

                ("evidence:" + p) |: all(
                    "status"          |: (!past && openForSale) ==>  (p.status      ?= OpenForSale),
                    "available today" |: (!past && openForSale) ==>  (p.vacant      ?= daylySale),
                    "total tickets"   |: (!past && openForSale) ==>  (p.available   ?= p.hall.seats - daysOnSale*daylySale)
                )
            },
            Parameters.default.withMaxDiscardRatio(50))
        }

        it("Consequently, 5 days before the show starts it is always sold out.") {
            check(forAll(genShow, arbitrary[LocalDate], arbitrary[LocalDate]){ (i:CvsRow, q:LocalDate, s:LocalDate) =>
                val p = scheduleDay(q,s,CvsRow.unapply(i).get)
                val past = inPast(s, q, i.date)
                val soldOut = s.minusDays(5) <= q

                ("evidence:" + p) |: all(
                    "status"          |: (!past && soldOut) ==>  (p.status      ?= SoldOut),
                    "available today" |: (!past && soldOut) ==>  (p.vacant      ?= 0),
                    "total tickets"   |: (!past && soldOut) ==>  (p.available   ?= 0)
                )
            },
            Parameters.default.withMaxDiscardRatio(150))
        }

        it("Shows always start in the big hall") {

            check(forAll(genShow, arbitrary[LocalDate], arbitrary[LocalDate]) { (i:CvsRow, q:LocalDate, s:LocalDate) =>
                val p = scheduleDay(q,s,CvsRow.unapply(i).get)
                val past = inPast(s, q, i.date)
                val big = s < i.date.plusDays(60)

                ("evidence:" + p) |: (!past && big) ==>  (p.hall      ?= Big())

            }, Parameters.default.withMaxDiscardRatio(30))

        }

        it("After 60 days, the show moves to the small hall") {
            check(forAll(genShow, arbitrary[LocalDate], arbitrary[LocalDate]) { (i:CvsRow, q:LocalDate, s:LocalDate) =>
                val p = scheduleDay(q,s,CvsRow.unapply(i).get)
                val past = inPast(s, q, i.date)
                val small = s >= i.date.plusDays(60)

                ("evidence:" + p) |: (!past && small) ==>  (p.hall      ?= Sml())

            }, Parameters.default.withMaxDiscardRatio(30))
        
        }

        it("After 80 days, the show price should be discounted with 20%"){
            def price(genre:Genre) = genre match {case Musical => 70; case Comedy => 50; case Drama => 40}

            check(forAll(genShow, arbitrary[LocalDate], arbitrary[LocalDate]) { (i:CvsRow, q:LocalDate, s:LocalDate) =>
                val p = scheduleDay(q,s,CvsRow.unapply(i).get)
                val past = inPast(s, q, i.date)
                val discounted = s > i.date.plusDays(80)

                ("discounted") |: (!past && discounted) ==>  (p.price ?= (price(p.genre)*0.8).toInt)
                
            }, Parameters.default.withMaxDiscardRatio(100))

            check(forAll(genShow, arbitrary[LocalDate], arbitrary[LocalDate]) { (i:CvsRow, q:LocalDate, s:LocalDate) =>
                val p = scheduleDay(q,s,CvsRow.unapply(i).get)
                val past = inPast(s, q, i.date)
                val regular = s <= i.date.plusDays(80)

                ("regular") |: (!past && regular) ==>  (p.price ?= price(p.genre))

            }, Parameters.default.withMaxDiscardRatio(100))
        }

    }

}
