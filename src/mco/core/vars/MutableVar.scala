package mco.core.vars

import cats.FlatMap
import cats.effect.Sync
import mco.syntax._
import monix.execution.atomic.{Atomic, AtomicBuilder}
import monix.execution.atomic.PaddingStrategy.NoPadding


/**
 * Var instance based on Monix atomic variable
 * @tparam F effect type of alterations (usually a Monad)
 * @tparam A value type
 */
class MutableVar[F[_]: Sync, A] private (state: Atomic[A])
  extends Var[F, A]
{
  override def apply(): F[A] = capture {
    state.get
  }

  override def :=(a: A): F[Unit] = capture {
    state.set(a)
  }

  override def ~=(f: A => A)(implicit F: FlatMap[F]): F[Unit] = capture {
    state.transform(f)
  }
}

object MutableVar {
  def apply[F[_]: Sync, A](initial: A)(implicit
    b: AtomicBuilder[A, _ <: Atomic[A]]
  ): F[Var[F, A]] = capture {
    val atomic = b.buildInstance(initial, NoPadding, allowPlatformIntrinsics = true)
    new MutableVar(atomic)
  }
}
