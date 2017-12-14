package mco.stubs

import cats._
import cats.syntax.applicative._
import mco.core.vars.Var


class ConstantVar[F[_]: Applicative, A](value: A) extends Var[F, A]{
  override def apply(): F[A] = value.pure[F]
  override def :=(a: A): F[Unit] = ().pure[F]
}
