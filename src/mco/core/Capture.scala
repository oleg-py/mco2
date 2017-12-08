package mco.core

import cats.effect.Sync

object Capture {
  /**
   * Enables syntax sugar of form `Capture { ??? }`
   */
  // TODO remove to something else
  def apply[F[_], A](a: => A)(implicit F: Sync[F]) = F.delay(a)
}
