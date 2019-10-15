package t4s
package store

/**
 * Key-value store API.
 */
trait Dba {
    def init(db:Map[String, Row])   :Unit
    def get(k:String)               :Either[Err, Row]
    def put(k:String, v:Row)        :Either[Err, Row]
    def entries()                   :Either[Err, Map[String, Row]]

    // looks like part of some service, but include in api for simplicity
    import java.time.LocalDate
    
    def loadDate(d1:LocalDate)(implicit reg:Regulation) : Either[Err, List[inventory.Performance]]
}
