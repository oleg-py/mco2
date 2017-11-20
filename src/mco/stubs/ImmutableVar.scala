package mco.stubs

import scalaz.Id._

import mco.core.vars.Var


class ImmutableVar[A](value: A) extends Var[Id, A]{
  override def apply(): A = value
  override def :=(a: A): Unit = ()
}
