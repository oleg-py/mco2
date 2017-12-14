package mco.core.state

import mco.core.ContentKind
import mco.core.paths.Path
import monocle.macros.Lenses


@Lenses case class ContentState(
  stamp: Stamp,
  assignedKind: ContentKind,
  target: Option[Path] = None
)
