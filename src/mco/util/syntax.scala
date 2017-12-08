package mco.util


object syntax {
  object any {
    implicit class AnyTypeSyntax[A](val a: A) extends AnyVal {
      @inline def pipe[B](f: A => B): B = f(a)
      @inline def tap[B](f: A => B): A = { f(a); a }
    }

    val unit: Unit = ()
  }

  @inline def ??[A](implicit a: A): A = a

  object map {
    implicit class MapMethods[A, B](val map: Map[A, B]) extends AnyVal {
      @inline def adjust(key: A, f: B => B): Map[A, B] =
        map.get(key).map(f).fold(map)(map.updated(key, _))

      @inline def mapKeys[C](f: A => C): Map[C, B] =
        map.map { case (k, v) => (f(k), v) }

      @inline def alter(key: A, f: Option[B] => Option[B]): Map[A, B] =
        f(map.get(key)).fold(map - key)(map.updated(key, _))
    }
  }
}
