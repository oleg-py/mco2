package mco.core.state

import mco.core.paths._
import mco.util.syntax.map._
import monocle.macros.Lenses


@Lenses case class ModState(
  stamp: Stamp,
  contents: Map[RelPath, ContentState]
) {
  def contentEnabled(key: RelPath): Boolean =
    contents.get(key).exists(_.stamp.enabled)

  def onResolve(targets: Vector[(RelPath, Path)], installed: Boolean): ModState = {
    val newContents = targets.foldLeft(contents) { case (map, (key, path)) =>
      map.adjust(key, _.onResolve(path, installed))
    }
    ModState(stamp.copy(installed = installed), newContents)
  }
}
