package mco.core.state

import scalaz._

// "State" part of State monad
trait Var[F[_], A] { outer =>
  def apply(): F[A]
  def :=(a: A): F[Unit]

  final def ~=(f: A => A)(implicit F: Bind[F]): F[Unit] =
    F.bind(F.map(this())(f))(:=)

  final def xmapF[B](to: A => F[B], from: B => F[A])(implicit F: Bind[F]): Var[F, B] =
    new Var[F, B] {
      def apply(): F[B] = F.bind(outer())(to)
      def :=(b: B): F[Unit] = F.bind(from(b))(outer := _)
    }
}

