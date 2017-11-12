package mco.core.state

import scalaz._
import syntax.foldable._
import syntax.std.option._

import mco.data.{Key, Path}
import monocle.macros.Lenses


@Lenses case class ModState(
  stamp: Stamp,
  contents: IMap[Key, ContentState]
) {
  def contentEnabled(key: Key): Boolean =
    contents.lookup(key).cata(_.stamp.enabled, false)

  def onResolve[F[_]: Foldable](targets: F[(Key, Path)], installed: Boolean) = {
    val newContents = targets.foldLeft(contents) { case (map, (key, path)) =>
      map.adjust(key, _.onResolve(path, installed))
    }
    ModState(stamp.copy(installed = installed), newContents)
  }
}
