package mco.core.vars

import scalaz._

import mco.data.Path
import mco.io.generic.Filesystem
import mco.io.generic.Filesystem._
import mco.util.serialize
import mco.util.syntax.fp._


class SerializedVar[F[_]: Applicative: Filesystem, A <: Serializable](
  target: Path,
  underlying: Var[F, A]
) extends Var[F, A] {
  override def apply(): F[A] = underlying()

  override def :=(a: A): F[Unit] =
    setBytes(target, serialize.to(a)) *>
      { underlying := a }
}

object SerializedVar {
  def apply[F[_]: Monad: Filesystem, A <: Serializable](
    target: Path,
    initial: => A,
    underlying: A => F[Var[F, A]]
  ): F[Var[F, A]] = {
    def read = getBytes(target).map(serialize.from[A])
    for {
      a <- exists(target).ifM(read, initial.point[F])
      _ <- setBytes(target, serialize.to(a))
      v <- underlying(a)
    } yield new SerializedVar(target, v): Var[F, A]
  }
}
