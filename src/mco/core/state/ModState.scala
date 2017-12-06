package mco.core.state

import scalaz._
import syntax.std.option._

import mco.core.paths._
import monocle.macros.Lenses


@Lenses case class ModState(
  stamp: Stamp,
  contents: IMap[RelPath, ContentState]
) {
  def contentEnabled(key: RelPath): Boolean =
    contents.lookup(key).cata(_.stamp.enabled, false)

  def onResolve(targets: Vector[(RelPath, Path)], installed: Boolean): ModState = {
    val newContents = targets.foldLeft(contents) { case (map, (key, path)) =>
      map.adjust(key, _.onResolve(path, installed))
    }
    ModState(stamp.copy(installed = installed), newContents)
  }
}
