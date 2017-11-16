package mco.stubs

import scalaz.Id._
import mco.core.Var


class ImmutableVar[A](value: A) extends Var[Id, A]{
  override def apply() = value
  override def :=(a: A) = ()
}
