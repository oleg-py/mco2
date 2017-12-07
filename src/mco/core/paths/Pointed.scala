package mco.core.paths

import cats.Comonad

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
  implicit val comonad: Comonad[Pointed] = new Comonad[Pointed] {
    override def extract[A](x: Pointed[A]): A = x.get

    override def coflatMap[A, B](fa: Pointed[A])(f: Pointed[A] => B): Pointed[B] =
      fa.coflatMap(f)

    override def map[A, B](fa: Pointed[A])(f: A => B): Pointed[B] =
      fa.replace(f(fa.get))
  }
}
