package mco.util

import scalaz._
import Id._

import monix.eval.Coeval


/**
 * Mixin allowing expressing effectful operations in terms of polymorphic
 * by-name `pure` without any additional constraints
 *
 * @tparam F - effect type to lift into
 */
trait Capture[F[_]] {
  def capture[A](a: => A): F[A]

  final def todo[A]: F[A] = capture { throw new NotImplementedError() }
}

/**
 * Basic implementations for IO-like types
 */
object Capture {

  // TODO - move to tests?
  trait Impure extends Capture[Id] {
    final def capture[A](a: => A): Id[A] = a
  }

  /**
   * Mixin wrapping values in monix.eval.Coeval
   */
  trait OfCoeval extends Capture[Coeval] {
    final def capture[A](a: => A): Coeval[A] = Coeval(a)
  }
}
