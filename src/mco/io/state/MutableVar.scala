package mco.io.state

import mco.core.state.Var
import mco.util.Capture


class MutableVar[F[_]: Capture, A](initial: A)
  extends Var[F, A]
{
  private var state: A = initial
  override def apply() = Capture { state }
  override def :=(a: A) = Capture { state = a }
}
