package mco.data

import scalaz.{Applicative, Comonad, Traverse}


case class Labelled[+A](key: Key, label: String, get: A) {
  def coflatMap[B](f: Labelled[A] => B): Labelled[B] = copy(get = f(this))
}

object Labelled {
  object of {
    def unapply[A](lb: Labelled[A]): Option[A] = Some(lb.get)
  }
  implicit val labelledInstance: Traverse[Labelled] = new Traverse[Labelled] with Comonad[Labelled] {
    override def traverseImpl[G[_], A, B](fa: Labelled[A])
      (f: (A) => G[B])(implicit G: Applicative[G]) = {
      G.map(f(fa.get))(el => fa.copy(get = el))
    }

    override def copoint[A](p: Labelled[A]): A = p.get
    override def cobind[A, B](fa: Labelled[A])(f: Labelled[A] => B): Labelled[B] =
      fa.coflatMap(f)
  }
}
