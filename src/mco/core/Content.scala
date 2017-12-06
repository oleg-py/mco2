package mco.core

import mco.core.paths._


sealed trait Content extends Product with Serializable {
  final def apply(key: RelPath): Keyed[Content] = paths.Keyed(key, this)
}

object Content {
  case object Document   extends Content
  case object Component  extends Content
  case object Unused     extends Content
}
