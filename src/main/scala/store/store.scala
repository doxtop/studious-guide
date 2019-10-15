package t4s
package store

import java.util.concurrent.atomic.AtomicReference
import java.time.LocalDate
import scala.collection.immutable._
import cats._, data._, syntax._, implicits._
import inventory._
import Core._

/** 
 * Simple scala types map based store.
 */
object Store extends Dba {
    // wrap in IO?
    val db:AtomicReference[Map[String,Row]] = new AtomicReference(Map.empty)
    
    def init(db1:Map[String, Row]) = {
        db.set(db1)
    }
    
    def get(k:String):Either[Err, Row] = {
        Either.catchOnly[NoSuchElementException] (db.get()(k)).leftMap(_ => ShowNotFound(k))
    }
    
    def put(k:String, v:Row):Either[Err, Row] = {
        db.set((db.get - k) ++ Map(k -> v))
        Either.right(v)
    }

    def entries():Either[Err, Map[String, Row]] = {
        Either.right(db.get)
    }

    /**
     * Create provided date schedule record in database if it not exist.
     */
    def loadDate(date:LocalDate)(implicit reg:Regulation):Either[Err, List[Performance]] = {
        val today = LocalDate.now

        val ent :Either[Err,Map[String, Row]] = entries()
        val rows:Either[Err,Row] = ent.map(_.values.toList.flatten)
        
        val z = rows.ensure(DayScheduleExist)(!_.exists{ case (`date`,_) => true; case _ => false })

        val mkDate:Either[Err,Row] = z.map(_ collect {
            case (`today`,v) => (date, scheduleDay(today, date, (v.title, v.air, v.genre)))
        })

        val entWithDate:Either[Err,Map[String,Row]] = mkDate
            .map(_.groupBy(_._2.title))
            .flatMap { dbase =>  ent.map { d2 =>  d2 |+| dbase} }

        // merge db eff and prepare return only one row    
        val eff:Either[Err,Row] = entWithDate.map{db => init(db); List.empty}

        // current date row
        (eff combine mkDate)
            .recoverWith{case DayScheduleExist => rows.map(_.filter(_._1 == date)) }
            .map(_.groupBy(_._2.title))
            .map(_.values.toList.flatten.map{case (d,v) => v})
    }

}
