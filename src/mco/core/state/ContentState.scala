package mco.core.state

import mco.core.Content
import mco.data.Path
import monocle.macros.Lenses


@Lenses case class ContentState(
  stamp: Stamp,
  target: Option[Path] = None,
  kindOverride: Option[Content.Plain] = None
)
