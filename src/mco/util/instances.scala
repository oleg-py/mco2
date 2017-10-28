package mco.util

import scalaz._

object instances {
  implicit def monoidForApplicative[F[_]: Applicative, M: Monoid]: Monoid[F[M]] =
    Monoid.liftMonoid[F, M]
}
