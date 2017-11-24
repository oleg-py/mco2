package mco.ui.props

import scalafx.beans.property.ObjectProperty

import mco.core.Capture
import mco.core.vars.Var

class PropertyBasedVar[F[_]: Capture, A](prop: ObjectProperty[A]) extends Var[F, A] {
  override def apply(): F[A] = Capture { prop() }
  override def :=(a: A): F[Unit] = Capture { prop() = a }
}
