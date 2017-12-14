package mco.core.vars

import cats.FlatMap
import cats.effect.Sync
import mco.core.Capture
import monix.execution.atomic.{Atomic, AtomicBuilder}
import monix.execution.atomic.PaddingStrategy.NoPadding


/**
 * Var instance based on Monix atomic variable
 * @param initial
 * @param ev$1
 * @param b
 * @tparam F effect type of alterations (usually a Monad)
 * @tparam A value type
 */
class MutableVar[F[_]: Sync, A](initial: A)(implicit
  b: AtomicBuilder[A, _ <: Atomic[A]]
)
  extends Var[F, A]
{
  private val state: Atomic[A] = b.buildInstance(
    initial,
    NoPadding,
    allowPlatformIntrinsics = true
  )
  override def apply() = Capture { state() }
  override def :=(a: A) = Capture { state() = a }
  override def ~=(f: A => A)(implicit F: FlatMap[F]) = Capture { state.transform(f) }
}
