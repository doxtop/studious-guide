package t4s

import java.time.LocalDate
import java.io.FileNotFoundException
import java.time.format.DateTimeParseException
import scala.io.Source

import com.typesafe.config.{ConfigFactory,ConfigException}
import cats._, data._, implicits._
import io.circe._, literal._, generic.auto._
import inventory._
import decode._
import Core._

/**
 * Command line interface to check show schedule for specific date.
 */
object Cli extends App with DecodeInventory {
    import scala.util.control.Exception._

    catching(classOf[NullPointerException], 
        classOf[ArrayIndexOutOfBoundsException],
        classOf[FileNotFoundException],
        classOf[DateTimeParseException],
        classOf[ConfigException]).either({
        
        implicit val reg:Regulation = new Reg(ConfigFactory.load.getConfig("regulation.limits"))

        val query   = LocalDate.parse(args(1))
        val show    = LocalDate.parse(args(2))

        case class ByGenre(genre:Genre, shows:List[Performance])
        
        val inv = fromFile(args(0)).done
            .option
            .foldMap(_.map(r => scheduleDay(query,show,CvsRow.unapply(r).get)))
            .groupBy(_.genre)
            .map((ByGenre.apply _).tupled(_))

        json"""{"inventory": $inv}"""

    }).fold(
        e => e match {
            case _ : ArrayIndexOutOfBoundsException => println(s"=> File name, query date and show date should be specified")
            case _ : FileNotFoundException          => println(s"File `${args(0)}` not found.")
            case pe: DateTimeParseException         => println(s"Dates should be ISO 8601 format (YYYY-MM-DD):\n==> ${pe.getMessage}")
            case ce: ConfigException                => println(s"Application not configured properly:\n==> ${ce.getMessage}")
            case _                                  => println(s"$e")
        },
        r => println(r)
    )
}
