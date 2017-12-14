package mco.core.vars

import cats._
import cats.syntax.functor._
import cats.syntax.flatMap._
import monocle.Lens


/**
 * Abstraction over state that can be altered on-demand
 *
 * Allows easy composition (via decoration) and separation
 * (using different variables) of stateful computations.
 *
 * A less side-effectful version of IORef / MVar, which
 * does not commit to a certain monad and allows
 * representation using e.g. StateT
 *
 * @tparam F effect type of alterations (usually a Monad)
 * @tparam A value type
 */
trait Var[F[_], A] { outer =>
  /**
   * Read current state of this Var
   * @return current state
   */
  def apply(): F[A]

  /**
   * Update state inside this Var
   * @param a new value to be set
   */
  def :=(a: A): F[Unit]

  /**
   * A shorthand operation to modifying the state
   * inside this Var with a provided function
   * @param f function used to transform contained value
   */
  def ~=(f: A => A)(implicit F: FlatMap[F]): F[Unit] =
    this().map(f) >>= { this := _ }

  /**
   * Create a new Var using monocle Lens which provides
   * access to corresponding part of the state
   */
  final def zoom[B](lens: Lens[A, B])(implicit F: FlatMap[F]): Var[F, B] =
    new Var[F, B] {
      override def apply(): F[B] = outer().map(lens.get)
      override def :=(b: B): F[Unit] = outer ~= lens.set(b)
    }

  /**
   * Create a new var using provided effectful functions
   */
  final def xmapF[B](to: A => F[B], from: B => F[A])(implicit F: FlatMap[F]): Var[F, B] =
    new Var[F, B] {
      override def apply(): F[B] = outer() >>= to
      override def :=(b: B): F[Unit] = from(b) >>= { outer := _ }
    }
}

