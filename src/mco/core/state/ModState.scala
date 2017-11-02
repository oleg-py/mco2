package mco.core.state

import scalaz._
import mco.data.Key
import monocle.macros.Lenses


@Lenses case class ModState(
  stamp: Stamp,
  contents: IMap[Key, ContentState]
)
