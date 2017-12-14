package mco.core.paths

import cats.Comonad

import monocle.Lens
import monocle.macros.GenLens


/**
 * Class representing value contained with a relative path
 *
 * @param key path associated with value
 * @param get value contained inside the pair
 * @tparam A type of value contained
 */
case class Pointed[+A](key: RelPath, get: A) {
  def map[B](f: A => B): Pointed[B] = copy(get = f(get))
}
