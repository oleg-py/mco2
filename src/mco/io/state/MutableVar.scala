package mco.io.state

import scalaz.Bind

import mco.core.Var
import mco.util.Capture
import monix.execution.atomic.PaddingStrategy.NoPadding
import monix.execution.atomic.{Atomic, AtomicBuilder}


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
  override def :=(a: A) = Capture { state := a }
  override def ~=(f: A => A)(implicit F: Bind[F]) = Capture { state.transform(f) }
}
