package t4s

import inventory._
import com.typesafe.config.Config

/**
 * Amsterdam ticket sales regulator.
 */
trait Regulation {
    def dayLimit(h:Hall):Either[Err, Int]
}

/**
 * Configuration based regulations
 */
class Reg(cfg:Config) extends Regulation {
    val big     = cfg.getInt("big")
    val small   = cfg.getInt("small")
    
    def dayLimit(h:Hall):Either[Err, Int] = h match {
        case Big() => Right(big)
        case Sml() => Right(small)
    }
}
