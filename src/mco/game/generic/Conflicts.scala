package mco.game.generic

import scala.collection.immutable.SortedMap
import scala.collection.mutable

import cats.syntax.foldable._
import cats.instances.all._
import mco.core.Status
import mco.core.paths.{Path, Pointed, RelPath}
import mco.core.state.RepoState


class Conflicts(state: RepoState) {
  private[this] val conflictTree =
    new mutable.HashMap[Path, mutable.TreeMap[Int, RelPath]]()
  // Conflict tree generation - only mutate map here
  for {
    ((_, mod), i)  <- state.orderedMods.iterator.zipWithIndex
    (rel, content) <- mod.contents
    path           <- content.target
  } {
    conflictTree.getOrElseUpdate(path, new mutable.TreeMap).
      update(i, rel)
  }
  // END conflict tree generation

  private def overrideIndex(path: Path, idx: Int) =
    conflictTree.get(path).flatMap(_.from(idx).drop(1).lastOption)

  private def recoveryIndex(path: Path, idx: Int) =
    conflictTree.get(path).flatMap(_.until(idx).lastOption)

  def resolutions(
    content: Vector[Path],
    idx: Int,
    status: Status
  ): (Set[Path], SortedMap[Int, Set[RelPath]]) =
    content foldMap { path =>
      val option = status match {
        case Status.Installed => overrideIndex(path, idx)
        case Status.Unused    => recoveryIndex(path, idx)
      }
      option foldMap { case (j, rel) =>
        (Set(path), SortedMap(j -> Set(rel)))
      }
    }
}
