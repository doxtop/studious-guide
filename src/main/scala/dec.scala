package t4s
package decode

import java.time._, format._
import atto._, Atto._
import cats.implicits._

import scala.util.Try
import scala.io.Source
import inventory._

case class CvsRow(title:String, date:LocalDate, genre:Genre)

/**
 * Inventory decoder.
 */
trait DecodeInventory {
    type Inventory = List[CvsRow]    

    val date: Parser[LocalDate] = take(10).flatMap { s =>
        Try(LocalDate.parse(s, DateTimeFormatter.ISO_LOCAL_DATE)).fold(ex => err(ex.getMessage), v => ok(v))
    }

    val genre: Parser[Genre] = {
        (char('"') ~> stringCI("drama")     <~ char('"')).map(_=> Drama     : Genre) |
        (char('"') ~> stringCI("comedy")    <~ char('"')).map(_=> Comedy    : Genre) |
        (char('"') ~> stringCI("musical")   <~ char('"')).map(_=> Musical   : Genre)
    }

    /**
     * From sample csv data it looks like original title was specified in reverse.
     * So regexp on Source lines may be expected
     * 
     * Some(l.reverse.split(",")) collect { case Array(t,date,name @ _*) =>
     *  (name.reverse.mkString(" ").reverse, LocalDate.parse(date.reverse,DateTimeFormatter.ISO_LOCAL_DATE), t)}
     * 
     * Simple stringLiteral used for the moment to avoid regexps.
     */
    val entry: Parser[CvsRow] = {
        (stringLiteral <~ char(','), date <~ char(','), genre).mapN(CvsRow.apply)
    }

    val inv:Parser[Inventory] = sepBy(entry, many(skip({(c:Char) => c match { case '\n' | '\r' => true; case _ => false}})))

    def fromSource(s:Source):ParseResult[Inventory] = {
        s.foldLeft(inv.parse("")){ (b, a) => {b.feed(a.toString)} }
    }

    def fromResource(name:String):ParseResult[Inventory] = {
        fromSource(Source.fromResource(name))
    }

    def fromFile(name:String):ParseResult[Inventory] = {
        fromSource(Source.fromFile(name))
    }

}
