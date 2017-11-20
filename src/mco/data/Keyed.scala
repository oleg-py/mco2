package mco.data

import monocle.Lens
import monocle.macros.GenLens

import scalaz.{Applicative, Comonad, Traverse}


/**
 * A glorified `Tuple2` containing a value together with arbitrary key
 *
 * @param key key associated with value
 * @param get value contained inside the pair
 * @tparam A type of value contained together witk ey
 */
case class Keyed[+A](key: Key, get: A) {
  def replace[B](b: B): Keyed[B] = copy(get = b)
  def coflatMap[B](f: Keyed[A] => B): Keyed[B] = copy(get = f(this))
}

object Keyed {
  def lens[A]: Lens[Keyed[A], A] = GenLens[Keyed[A]](_.get)
  implicit val labelledInstance: Traverse[Keyed] with Comonad[Keyed] =
    new Traverse[Keyed] with Comonad[Keyed] {
      override def traverseImpl[G[_], A, B](fa: Keyed[A])
        (f: (A) => G[B])(implicit G: Applicative[G]) = {
        G.map(f(fa.get))(el => fa.copy(get = el))
      }

      override def copoint[A](p: Keyed[A]): A = p.get
      override def cobind[A, B](fa: Keyed[A])(f: Keyed[A] => B): Keyed[B] =
        fa.coflatMap(f)
    }
}
