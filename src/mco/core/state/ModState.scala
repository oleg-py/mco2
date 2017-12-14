package mco.core.state

import mco.core.paths._
import mco.syntax._
import monocle.macros.Lenses


@Lenses case class ModState(
  stamp: Stamp,
  contents: Map[RelPath, ContentState]
)
