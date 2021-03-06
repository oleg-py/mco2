package mco

import cats.Applicative
import cats.effect.Sync
import cats.kernel.Monoid


object syntax {
  @inline def ifM[F[_]: Applicative, A: Monoid](b: Boolean)(ifTrue: => F[A]): F[A] =
    if (b) ifTrue
    else Applicative[F].pure(Monoid[A].empty)

  implicit class MapMethods[A, B](val map: Map[A, B]) extends AnyVal {
    @inline def adjust(key: A, f: B => B): Map[A, B] =
      map.get(key).map(f).fold(map)(map.updated(key, _))

    @inline def alter(key: A, f: Option[B] => Option[B]): Map[A, B] =
      f(map.get(key)).fold(map - key)(map.updated(key, _))
  }

  @inline def capture[F[_], A](thunk: => A)(implicit F: Sync[F]): F[A] =
    F.delay(thunk)
}
