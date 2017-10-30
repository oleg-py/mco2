package mco.core

import scalaz._
import std.vector._

import mco.data.{Key, Keyed}


sealed trait Content extends Product with Serializable

object Content {
  case class Container(children: Vector[Keyed[Content]])
    extends Content

  sealed trait Plain extends Content {
    final def apply(key: Key): Keyed[this.type] = Keyed(key, this)
  }

  case object Document   extends Plain
  case object Component  extends Plain
  case object Unused     extends Plain
}
