package mco.util

import scalaz._
import scalaz.syntax.std.option._
import scalaz.syntax.std.boolean._

trait Interrupt[F[_]] {
  def abortImpl[A](msg: String): F[A]

  final def getOrAbort[A](a: Option[A], msg: String = "Invariant violation")(
    implicit F: Applicative[F]
  ): F[A] = a.cata(F.point(_), abortImpl[A](msg))

  final def abortIf(bool: Boolean)(msg: String)(implicit F: Applicative[F]): F[Unit] =
    bool.fold(abortImpl[Unit](msg), F.point(()))
}

