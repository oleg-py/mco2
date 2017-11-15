package mco.ui

import scalafx.beans.binding.Bindings
import scalafx.beans.property.ObjectProperty
import scalaz._

import mco.util.PropsImplementation

import javafx.beans.{InvalidationListener, Observable}


package object props extends PropsImplementation {
  implicit val propInstances: Monad[Prop] with Comonad[Prop] with Foldable1[Prop] =
    new Monad[Prop] with Comonad[Prop] with Foldable1[Prop] {
      override def copoint[A](p: Prop[A]) = p.value

      override def cobind[A, B](fa: Prop[A])(f: Prop[A] => B): Prop[B] =
        Bindings.createObjectBinding(() => f(fa), fa)

      override def point[A](a: => A) = ObjectProperty(a)

      override def bind[A, B](fa: Prop[A])(f: A => Prop[B]) = join(map(fa)(f))

      override def foldMapRight1[A, B](fa: Prop[A])(z: A => B)(f: (A, => B) => B): B =
        z(fa.value)

      override def foldMap1[A, B](fa: Prop[A])(f: A => B)(implicit F: Semigroup[B]) =
        f(fa.value)

      override def join[A](ffa: Prop[Prop[A]]) = {
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
        Bindings.createObjectBinding(() => f(fa.value), fa)
      }

      override def apply2[A, B, C](fa: => Prop[A], fb: => Prop[B])(f: (A, B) => C) = {
        val sfa = fa
        val sfb = fb
        Bindings.createObjectBinding(() => f(sfa.value, sfb.value), sfa, sfb)
      }
    }

  implicit class PropOps[A] (val p: Prop[A]) {
    def asProp = p
  }
}
