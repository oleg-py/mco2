package mco.ui.props

import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty

import cats.effect.Sync
import mco.core.vars.Var
import mco.syntax._

class PropertyBasedVar[F[_]: Sync, A](prop: ObjectProperty[A]) extends Var[F, A] {
  override def apply(): F[A] = capture {
    prop()
  }

  override def :=(a: A): F[Unit] = capture {
    Platform.runLater { prop() = a }
  }
}
