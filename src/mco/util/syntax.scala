package mco.util

import scalaz.syntax._
import scalaz.syntax.std.{ToOptionIdOps, ToOptionOps}


object syntax {
  object any {
    implicit class AnyTypeSyntax[A](val a: A) extends AnyVal {
      @inline def pipe[B](f: A => B): B = f(a)
      @inline def tap[B](f: A => B): A = { f(a); a }
    }

    val unit: Unit = ()
  }

  /**
   * Scalaz has functor syntax in both traverse ops & monad ops
   * which prevents it from working. This is probably most useful
   * combination of imports that is not `syntax.all` yet.
   */
  object fp extends ToMonadErrorOps
    with ToTraverseOps
    with ToMonoidOps
    with ToOptionOps
    with ToOptionIdOps
    with ToComonadOps

  @inline def ??[A](implicit a: A): A = a

  object map {
    implicit class MapMethods[A, B](val map: Map[A, B]) extends AnyVal {
      @inline def adjust(key: A, f: B => B): Map[A, B] =
        map.get(key).map(f).fold(map)(map.updated(key, _))

      @inline def mapKeys[C](f: A => C): Map[C, B] =
        map.map { case (k, v) => (f(k), v)}
    }
  }
}
