package mco.data

import scalaz.{Applicative, Comonad, Traverse}


case class Keyed[+A](key: Key, get: A) {
  def coflatMap[B](f: Keyed[A] => B): Keyed[B] = copy(get = f(this))
}

object Keyed {
  implicit val labelledInstance: Traverse[Keyed] = new Traverse[Keyed] with Comonad[Keyed] {
    override def traverseImpl[G[_], A, B](fa: Keyed[A])
      (f: (A) => G[B])(implicit G: Applicative[G]) = {
      G.map(f(fa.get))(el => fa.copy(get = el))
    }

    override def copoint[A](p: Keyed[A]): A = p.get
    override def cobind[A, B](fa: Keyed[A])(f: Keyed[A] => B): Keyed[B] =
      fa.coflatMap(f)
  }
}
