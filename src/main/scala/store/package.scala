package t4s

import java.time.LocalDate

package object store {

    type Row = List[(LocalDate, inventory.Performance)]

    type Res[T] = Either[Err, T]
}
