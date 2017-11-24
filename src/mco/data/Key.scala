package mco.data

import scalaz._
import scalaz.std.string._


/**
 * Class used to represent unique map keys without particular
 * semantic value
 * @param unwrap internal string representation
 */
// TODO: Unwrapping means there's some semantic meaning to keys. Should be removed
// TODO: replace key with segment
case class Key(unwrap: String) extends AnyVal

object Key {
  implicit val order: Order[Key] = Order.orderBy(_.unwrap)
}
