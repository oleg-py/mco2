package mco.util

import cats._

object instances {
  implicit def monoidForApplicative[F[_]: Applicative, M: Monoid]: Monoid[F[M]] =
    Applicative.monoid[F, M]
}
