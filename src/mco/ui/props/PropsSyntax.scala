package mco.ui.props

import scalafx.beans.property.Property
import scalafx.beans.value.ObservableValue

import javafx.beans.{InvalidationListener, Observable}
import javafx.beans.property.{ObjectProperty => JObjectProperty}
import javafx.collections.ObservableList

trait PropsSyntax { this: PropsImplementation =>
  implicit class AsPropSyntax[A](self: A) {
    def asProp[B](implicit view: A => Prop[B]) = view(self)
  }

  implicit class PropSyntax[A](self: Prop[A]) {
    @inline def refine[J](implicit ev: A ToJavaFxAs J): ObservableValue[A, J] =
      self.asInstanceOf[ObservableValue[A, J]]

    def foreach[B](cb: A => Unit): Unit = {
      cb(self())
      self onChange {
        cb(self())
      }
      ()
    }
  }

  implicit class PropertyBindingToPropSyntax[A, J](val self: Property[A, J]) {
    def <== (ov1: Prop[A])(implicit ev: A ToJavaFxAs J): Unit = self <== ov1.refine
  }

//  implicit class JavaPropertyBindingToPropSyntax[J](val self: JObjectProperty[J]) {
//    def <==[A] (ov1: Prop[A])(implicit conv: A => J): Unit =
//      self.bind(ov1.map(conv).refine[J].delegate)
//  }

  implicit class ObservableListBinding[J](val self: ObservableList[J]) {
    def <==[A] (ov1: Prop[_ <: Seq[A]])(implicit conv: A => J): Unit = {
      ov1.foreach(syncLists(self, _))
    }
  }

  implicit class ObservableListPropertyBinding[J](val self: JObjectProperty[ObservableList[J]]) {
    def <==[A] (ov1: Prop[_ <: Seq[A]])(implicit conv: A => J): Unit = {
      ov1.foreach(syncLists(self.get, _))

      self.addListener(new InvalidationListener {
        override def invalidated(observable: Observable): Unit = {
          syncLists(self.get, ov1())
        }
      })
    }
  }

  @inline private def syncLists[A, J](
    ol: ObservableList[J],
    seq: Seq[A]
  )(implicit conv: A => J) = {
    val iter = seq.iterator
    var idx = 0
    while (idx < ol.size && iter.hasNext) {
      val el = iter.next()
      if (el != ol.get(idx)) {
        ol.set(idx, el)
      }
      idx += 1
    }
    while (iter.hasNext) {
      ol.add(iter.next())
      idx += 1
    }
    if (idx < ol.size) {
      ol.remove(idx, ol.size())
    }
  }
}
