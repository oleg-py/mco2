package mco.data

import scalaz._
import scalaz.std.string._

// TODO: Unwrapping means there's some semantic meaning to keys. That's horrible
case class Key(unwrap: String) extends AnyVal

object Key {
  implicit val order: Order[Key] = Order.orderBy(_.unwrap)
}
