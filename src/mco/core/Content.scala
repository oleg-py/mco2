package mco.core

import scalaz._
import std.vector._

import mco.data.{Key, Labelled}


sealed trait Content extends Product with Serializable

object Content {
  case class Container(children: Vector[Labelled[Content]])
    extends Content

  sealed trait Plain extends Content {
    final def apply(key: Key, label: String): Labelled[this.type] = Labelled(key, label, this)
  }

  case object Document   extends Plain
  case object Component  extends Plain
  case object Unused     extends Plain
}
