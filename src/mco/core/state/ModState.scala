package mco.core.state

import scalaz._

import mco.data.Key


case class ModState(
  stamp: Stamp,
  contents: IMap[Key, ContentState]
)
