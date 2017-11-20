package mco.core.vars

import scalaz.Bind

import mco.util.Capture
import monix.execution.atomic.{Atomic, AtomicBuilder}
import monix.execution.atomic.PaddingStrategy.NoPadding


class MutableVar[F[_]: Capture, A](initial: A)(implicit
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
  override def ~=(f: A => A)(implicit F: Bind[F]) = Capture { state.transform(f) }
}
