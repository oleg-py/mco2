package mco.core.paths

import scalaz.{Applicative, Comonad, Traverse}

import monocle.Lens
import monocle.macros.GenLens


/**
 * A glorified `Tuple2` containing a value together with a relative path
 *
 * @param key path associated with value
 * @param get value contained inside the pair
 * @tparam A type of value contained
 */
case class Pointed[+A](key: RelPath, get: A) {
  def replace[B](b: B): Pointed[B] = copy(get = b)
  def coflatMap[B](f: Pointed[A] => B): Pointed[B] = copy(get = f(this))
}

object Pointed {
  def lens[A]: Lens[Pointed[A], A] = GenLens[Pointed[A]](_.get)
  implicit val labelledInstance: Traverse[Pointed] with Comonad[Pointed] =
    new Traverse[Pointed] with Comonad[Pointed] {
      override def traverseImpl[G[_], A, B](fa: Pointed[A])
        (f: (A) => G[B])(implicit G: Applicative[G]) = {
        G.map(f(fa.get))(el => fa.copy(get = el))
      }

      override def copoint[A](p: Pointed[A]): A = p.get
      override def cobind[A, B](fa: Pointed[A])(f: Pointed[A] => B): Pointed[B] =
        fa.coflatMap(f)
    }
}
