package t4s

class Sp extends org.scalatest.FunSpec {
    import cats._, data._, implicits._

    sealed trait E
    case object E1 extends E 
    
    /** 
     * Left type of either will be evicted, so we need to state them directly
     * in `fromEither` and `fromOption` or it will not compile.
     */
    it("should resolve either left type") {
        assert((for {
            // this will not compile without specify E1:E 
            x  <- EitherT.fromEither[Id](Either.right(5))
            y  <- EitherT.cond[Id](true, x, E1:E)
        } yield y).value == Right(5))
    }
}
