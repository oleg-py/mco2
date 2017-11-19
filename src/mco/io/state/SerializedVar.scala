package mco.io.state

import scalaz._

import mco.core.Var
import mco.data.Path
import mco.io.generic.Filesystem
import mco.util.syntax.fp._
import mco.util.serialize
import Filesystem._


class SerializedVar[F[_]: Applicative: Filesystem, A <: Serializable](
  target: Path,
  underlying: Var[F, A]
) extends Var[F, A] {
  override def apply(): F[A] = underlying()
  override def :=(a: A): F[Unit] = {
    setBytes(target, serialize.to(a)) *>
      { underlying := a }
  }
}

object SerializedVar {
  def apply[F[_]: Monad: Filesystem, A <: Serializable](
    target: Path,
    initial: F[A],
    underlying: A => Var[F, A]
  ): F[Var[F, A]] = {
    def read = getBytes(target).map(serialize.from[A])
    for {
      a <- exists(target).ifM(read, initial)
      _ <- setBytes(target, serialize.to(a))
    } yield new SerializedVar(target, underlying(a)): Var[F, A]
  }
}
