package t4s
package inventory

import java.time._, temporal._, format._
import org.scalacheck._, Arbitrary._, Prop._
import cats._, data._, implicits._

import decode._

/**
    Generators to create inventory.
*/
trait GenInventory {
    
    def dateGen:Gen[LocalDate] = 
        //Gen.choose(959900000000l, 999300000000l)      // summer 2001
        //Gen.choose(978310000000l, 1009800000000l)     // whole 2001
        //Gen.choose(978310000000l, 1577900000000l)     // 2001 - 2020
        Gen.choose(0L, Long.MaxValue/300000000)         // 50 years since 1970
        .map(Instant.ofEpochMilli(_).atZone(ZoneId.systemDefault()).toLocalDate)

    implicit val arbitraryDate:Arbitrary[LocalDate] = Arbitrary(dateGen)

    implicit val dateOrder:Order[LocalDate] = Order.fromOrdering[LocalDate](Ordering.by(_.toEpochDay))

    def genShow:Gen[CvsRow] = for {
        title   <- Gen.listOfN(10, Gen.alphaChar).map(_.mkString)
        date    <- arbitrary[LocalDate]
        genre   <- Gen.oneOf(Musical, Drama, Comedy)
    } yield CvsRow(title, date, genre)

    def genPerf:Gen[Performance] = for {
        title   <- Gen.listOfN(10, Gen.alphaChar).map(_.mkString)
        status  <- Gen.oneOf(NotStarted, OpenForSale, SoldOut, InThePast)
        genre   <- Gen.oneOf(Musical, Drama, Comedy)
        air     <- arbitrary[LocalDate]
        hall    <- Gen.oneOf(Sml(),Big())
        vacant  <- Gen.posNum[Int]
        left    <- Gen.posNum[Int]
        price   <- Gen.posNum[Int]
    } yield Performance(genre, title, status, air, vacant, left, hall, price)

    implicit lazy val arbitraryPerformance:Arbitrary[Performance] = Arbitrary(genPerf)

    def showList = Gen.nonEmptyContainerOf[List, Performance](genPerf)

}
