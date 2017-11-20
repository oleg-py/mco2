package mco.core.vars

import scalaz._
import scalaz.syntax.bind._


/**
 * "State" part of state monad
 *
 * Allows easy composition (via decoration) and separation
 * (using different variables) of stateful computations
 *
 * Sane implementations ensure that { var0 := a } >> var0()
 * is equal to a.pure[F]
 *
 * @tparam F effect type
 * @tparam A value type
 */
trait Var[F[_], A] { outer =>
  def apply(): F[A]
  def :=(a: A): F[Unit]

  def ~=(f: A => A)(implicit F: Bind[F]): F[Unit] =
    this().map(f) >>= { this := _ }

  final def xmapF[B](to: A => F[B], from: B => F[A])(implicit F: Bind[F]): Var[F, B] =
    new Var[F, B] {
      override def apply(): F[B] = outer() >>= to
      override def :=(b: B): F[Unit] = from(b) >>= { outer := _ }
    }
}

