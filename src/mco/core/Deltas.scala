package mco.core

import mco.data.Key


object Deltas {
  case class OfContent(enabled: Boolean, kindOverride: Option[Content.Plain])
  case class OfMod(installed: Boolean, contents: Map[Key, OfContent])
}
