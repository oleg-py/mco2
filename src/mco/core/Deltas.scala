package mco.core

import mco.data.Key


object Deltas {
  case class OfContent(
    enabled: Option[Boolean] = None,
    kindOverride: Option[Option[Content.Plain]] = None
  )
  case class OfMod(
    installed: Option[Boolean] = None,
    contents: Map[Key, OfContent] = Map()
  )
}
