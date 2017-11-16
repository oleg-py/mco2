package mco.io.state

import scalaz._
import scalaz.syntax.bind._

import better.files._
import mco.core.Var
import mco.data.Path
import mco.util.Capture

// TODO - use Filesystem, not Capture
class SerializedVar[F[_]: Apply: Capture, A <: Serializable](
  target: Path,
  underlying: Var[F, A]
) extends Var[F, A] {
  override def apply(): F[A] = underlying()
  override def :=(a: A): F[Unit] = Capture {
    File(target.asString).writeSerialized(a)
  } *> { underlying := a }
}

object SerializedVar {
  def apply[F[_]: Capture: Bind, A <: Serializable](
    target: Path,
    initial: F[A],
    underlying: A => Var[F, A]
  ): F[Var[F, A]] = {
    Capture {
      val file = File(target.asString)
      if (file.exists) Capture { file.readDeserialized[A] }
      else initial
    }
      .join
      .map(underlying)
      .>>! { var0 =>
        for {
          value <- var0()
          _ <- Capture { File(target.asString).writeSerialized(value) }
        } yield ()
      }
      .map(new SerializedVar(target, _))
      .widen
  }
}
