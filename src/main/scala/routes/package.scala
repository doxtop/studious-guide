package t4s

import java.time.LocalDate
import scala.language.higherKinds
import scala.util.Try

import store._
import inventory._

package object routes {
    import cats._, data._, implicits._, effect._
    import io.circe._, literal._, generic.auto._
    import org.http4s._, org.http4s.dsl.io._
    
    object LocalDateVar {
        def unapply(str:String): Option[LocalDate] = {
            if(!str.isEmpty) 
                Try(LocalDate.parse(str)).toOption 
            else None
        }
    }

    /** 
     * Serve web site static. 
     */
    val static = HttpService[IO] {
        case req @ GET -> Root / path if List(
            "index.html", "favicon.ico", "style.css",
            "react.js", "react-dom.js", "create-react-class.js", 
            "app.js","require.js").contains(path) =>
            StaticFile.fromResource("/" +path, Some(req)).getOrElseF(NotFound()) }

    /** 
     * Circle specific class to make encoding easier 
     * doesn't have direct encoders, suppused to use circle literal instead
     */
    case class ByGenre(genre:Genre, shows:List[Performance])

    /**
     * Encode typical service route response to json with circe literals.
     */
    def encodeRes(list: Either[Err, List[Performance]]):IO[Response[IO]] = 
        list.map(_.groupBy(_.genre))
            .map(_.map((ByGenre.apply _).tupled(_)))
            .fold( e => json"""{"error":     $e }"""
                 , r => json"""{"inventory": $r }""")
            .pure[IO]
            .flatMap{(j:Json) => Ok(j.noSpaces)}

    // `all` will have a lot of name clashes
    object inv extends InventoryRoute with TicketsRoute
}
