package mco.core.state

import mco.core.Content
import mco.data.Path


case class ContentState(
  stamp: Stamp,
  target: Vector[Path] = Vector.empty,
  kindOverride: Option[Content.Plain] = None
)
