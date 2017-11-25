package mco.core

import mco.data.paths.RelPath
import mco.data.Keyed


sealed trait Content extends Product with Serializable {
  final def apply(key: RelPath): Keyed[this.type] = Keyed(key, this)
}

object Content {
  case object Document   extends Content
  case object Component  extends Content
  case object Unused     extends Content
}
