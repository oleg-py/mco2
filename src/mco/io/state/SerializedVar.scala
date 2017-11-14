package mco.io.state

import scalaz._
import scalaz.syntax.bind._

import mco.core.state.Var
import mco.data.Path
import mco.util.Capture
import better.files._

class SerializedVar[F[_]: Apply, A <: Serializable](
  target: Path,
  underlying: Var[F, A]
)(
  implicit capture: Capture[F]
) extends Var[F, A] {
  override def apply() = underlying()
  override def :=(a: A) = capture {
    File(target.asString).writeSerialized(a)
  } *> { underlying := a }
}

object SerializedVar {
  def apply[F[_]: Capture: Bind, A <: Serializable](
    target: Path,
    initial: F[A],
    underlying: A => Var[F, A]
  ): F[Var[F, A]] = {
    val capture = implicitly[Capture[F]]
    capture {
      val file = File(target.asString)
      if (file.exists) capture { file.readDeserialized[A] }
      else initial
    }
      .join
      .map(underlying)
      .map(new SerializedVar(target, _))
      .widen
  }
}
