package mco.core

import mco.data.paths.RelPath
import mco.data.Keyed


sealed trait Content extends Product with Serializable

object Content {
  case class Container(children: Vector[Keyed[Content]])
    extends Content

  sealed trait Plain extends Content {
    final def apply(key: RelPath): Keyed[this.type] = Keyed(key, this)
  }

  case object Document   extends Plain
  case object Component  extends Plain
  case object Unused     extends Plain
}
