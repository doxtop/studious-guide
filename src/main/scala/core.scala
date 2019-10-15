package t4s

import inventory._

object Core {
    import java.time._, temporal._, format._
    import scala.math._

    /** Make schedule day */
    def scheduleDay(query:LocalDate,show:LocalDate,inv:Tuple3[String,LocalDate,Genre])(implicit reg:Regulation) = {
    
        val start           = inv._2
        val small           = start.plusDays(60)
        val discount        = start.plusDays(80)
        val end             = start.plusDays(100)
        val sales           = show.minusDays(25)
        val soldOut         = sales.plusDays(20)

        val status:Sale     = if (show.isBefore(query) || show.isAfter(end)) InThePast
                         else if (query.isBefore(sales))  NotStarted
                         else if (query.isAfter(soldOut.minusDays(1))) SoldOut
                         else OpenForSale

        val hall:Hall       = if (show.isAfter(small)  || show.isEqual(small)) Sml() else Big()

        val size:Int        = (hall, status) match {
            case (_, InThePast) => 0
            case (s:Hall, _)    => s.seats
        }

        val dailyLim:Int    = status match {
            case InThePast  => 0
            case _          => reg.dayLimit(hall).getOrElse(0)
        }
        
        val activeDays      = min(20, max(0, sales.until(query, ChronoUnit.DAYS).toInt))
        var available       = size - activeDays * dailyLim
        var left            = if (query.isAfter(sales.minusDays(1)) && query.isBefore(soldOut) ) dailyLim else 0

        val ratio :Double    = if (show.isAfter(discount)) 0.8 else 1 
        val tariff:Double    = inv._3 match {case Musical => 70; case Comedy => 50; case Drama => 40}
        val price :Int       = (ratio * tariff).intValue
        
        Performance(inv._3, inv._1, status, inv._2, left, available, hall, price)
    }

}
