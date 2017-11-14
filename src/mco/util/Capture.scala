package mco.util

import scalaz._
import Id._

import monix.eval.Coeval


/**
 * Typeclass allowing expressing effectful operations in terms of polymorphic
 * by-name `pure` without any additional constraints
 *
 * @tparam F - effect type to lift into
 */
trait Capture[F[_]] {
  def apply[A](a: => A): F[A]
  final def todo[A]: F[A] = apply { throw new NotImplementedError() }
}

/**
 * Basic implementations for IO-like types
 */
object Capture {
  def apply[F[_], A](a: => A)(implicit C: Capture[F]) = C { a }
  object coeval {
    implicit val captureOfCoeval: Capture[Coeval] = new Capture[Coeval] {
      override def apply[A](a: => A): Coeval[A] = Coeval(a)
    }
  }
}
