package mco.ui.props

import scalaz.Id._
import scalafx.beans.property.ObjectProperty

import mco.core.vars.Var

class PropertyBasedVar[A](prop: ObjectProperty[A]) extends Var[Id, A] {
  override def apply(): A = prop()
  override def :=(a: A): Unit = prop() = a
}
