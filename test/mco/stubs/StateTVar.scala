package mco.stubs

import cats.Applicative
import cats.data.StateT
import mco.core.vars.Var


class StateTVar[F[_]: Applicative, A] extends Var[StateT[F, A, ?], A] {
  override def apply(): StateT[F, A, A] = StateT.get[F, A]
  override def :=(a: A): StateT[F, A, Unit] = StateT.set[F, A](a)
}
