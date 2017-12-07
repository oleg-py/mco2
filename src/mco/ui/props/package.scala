package mco.ui

import scala.annotation.tailrec
import scalafx.beans.property.ObjectProperty

import cats._

import javafx.beans.{InvalidationListener, Observable}


package object props extends PropsImplementation with PropsSyntax {
  implicit val propInstances: Monad[Prop] = new Monad[Prop] {

    override def flatMap[A, B](fa: Prop[A])(f: A => Prop[B]): Prop[B] =
      flatten(map(fa)(f))

    // TODO this does not watch
    @tailrec override def tailRecM[A, B](a: A)(f: A => Prop[Either[A, B]]): Prop[B] =
      f(a).value match {
        case Left(l) => tailRecM(l)(f)
        case Right(r) => pure(r)
      }

    override def pure[A](x: A): Prop[A] = ObjectProperty(x)


    override def flatten[A](ffa: Prop[Prop[A]]) = {
      var fa = ffa.value
      val property = ObjectProperty(fa.value)

      val watchB = new InvalidationListener {
        override def invalidated(observable: Observable): Unit = {
          property.value = fa.value
        }
      }
      fa.addListener(watchB)

      ffa.onChange {
        fa.removeListener(watchB)
        fa = ffa.value
        fa.addListener(watchB)
        property.value = fa.value
      }
      property
    }

    override def map[A, B](fa: Prop[A])(f: A => B) = {
      val property = ObjectProperty(f(fa()))
      val watch: InvalidationListener = (_) => { property() = f(fa()) }
      fa.addListener(watch)
      property
    }


    override def map2[A, B, Z](fa: Prop[A], fb: Prop[B])(f: (A, B) => Z): Prop[Z] = {
      val sfa = fa
      val sfb = fb
      val property = ObjectProperty(f(sfa(), sfb()))
      val watch: InvalidationListener = (_) => { property() = f(sfa(), sfb()) }
      sfa.addListener(watch)
      sfb.addListener(watch)
      property
    }
  }
}
