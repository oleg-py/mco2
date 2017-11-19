package mco.io.state

import scalaz.{Bind, MonadState}

import mco.core.Var


class MonadicVar[F[_], A](implicit F: MonadState[F, A])
  extends Var[F, A]
{
  override def apply() = F.get
  override def :=(a: A) = F.put(a)
  override def ~=(f: A => A)(implicit ev: Bind[F]) = F.modify(f)
}
