package mco.util

import monix.eval.Coeval


/**
 * Typeclass allowing expressing effectful operations in terms of polymorphic
 * by-name `pure` without any additional constraints
 *
 * @tparam F - effect type to lift into
 */
trait Capture[F[_]] {
  def apply[A](a: => A): F[A]
}

/**
 * Basic implementations for IO-like types
 */
object Capture {
  /**
   * Enables syntax sugar of form `Capture { ??? }` whenever
   * implicit Capture instance is in scope
   */
  def apply[F[_], A](a: => A)(implicit C: Capture[F]) = C { a }

  /**
   * Implementation of Capture for monix.eval.Coeval
   */
  object coeval {
    implicit val captureOfCoeval: Capture[Coeval] = new Capture[Coeval] {
      override def apply[A](a: => A): Coeval[A] = Coeval(a)
    }
  }
}
