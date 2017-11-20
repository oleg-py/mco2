package mco.core.vars

import scalaz.{Bind, MonadState}


class MonadicVar[F[_], A](implicit F: MonadState[F, A])
  extends Var[F, A]
{
  override def apply(): F[A] =
    F.get

  override def :=(a: A): F[Unit] =
    F.put(a)

  override def ~=(f: A => A)(implicit ev: Bind[F]): F[Unit] =
    F.modify(f)
}
