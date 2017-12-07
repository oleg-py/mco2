package mco.core.vars

import cats._
import cats.syntax.functor._
import cats.syntax.flatMap._
import monocle.Lens


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

  def ~=(f: A => A)(implicit F: FlatMap[F]): F[Unit] =
    this().map(f) >>= { this := _ }

  final def zoom[B](lens: Lens[A, B])(implicit F: FlatMap[F]): Var[F, B] =
    new Var[F, B] {
      override def apply(): F[B] = outer().map(lens.get)
      override def :=(b: B): F[Unit] = outer ~= lens.set(b)
    }

  final def xmapF[B](to: A => F[B], from: B => F[A])(implicit F: FlatMap[F]): Var[F, B] =
    new Var[F, B] {
      override def apply(): F[B] = outer() >>= to
      override def :=(b: B): F[Unit] = from(b) >>= { outer := _ }
    }
}

