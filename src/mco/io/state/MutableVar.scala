package mco.io.state

import mco.core.state.Var
import mco.util.Capture


class MutableVar[F[_], A](initial: A)(implicit capture: Capture[F])
  extends Var[F, A]
{
  private var state: A = initial
  override def apply() = capture { state }
  override def :=(a: A) = capture { state = a }
}
