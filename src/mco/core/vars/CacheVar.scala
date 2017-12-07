package mco.core.vars

import cats._
import cats.syntax.all._

class CacheVar[F[_]: Apply, A](base: Var[F, A], reads: Var[F, A])
  extends Var[F, A]
{
  override def apply(): F[A] = reads()
  override def :=(a: A): F[Unit] = (base := a) *> (reads := a)
}

object CacheVar {
  def apply[F[_], A, E](
    read: => F[A]
  )(
    base: Var[F, A],
    mkReads: A => F[Var[F, A]]
  )(implicit
    F: MonadError[F, E]
  ): F[Var[F, A]] =
    base().handleErrorWith(_ => read)
      .flatMap(mkReads)
      .map(new CacheVar(base, _))
}