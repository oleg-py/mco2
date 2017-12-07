package mco.core

import cats._

import monix.eval.Coeval


/**
 * Typeclass allowing importing effects into
 * possibly pure context F
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

  /**
   * Implementation of Capture for Id (identity type)
   *
   * Allows execution of unbounded side effects. Useful for REPL testing.
   */
  object yolo {
    implicit val captureOfId: Capture[Id] = new Capture[Id] {
      override def apply[A](a: => A): A = a
    }
  }
}
