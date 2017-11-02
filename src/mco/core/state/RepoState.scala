package mco.core.state

import scalaz._
import std.vector._
import std.option._
import std.anyVal._
import std.map._
import syntax.std.boolean._
import mco.data.{Keyed, Path}
import mco.util.syntax.fp._
import monocle.macros.Lenses


@Lenses case class RepoState (
  orderedMods: Vector[Keyed[ModState]]
) {
  lazy val conflicts: Map[Path, ISet[Int]] = {
    orderedMods.indexed.foldMap { case (i, Keyed(_, mod)) =>
      mod.stamp.enabled ?? mod.contents.foldMap { cState =>
        cState.stamp.enabled ?? cState.target.strengthR(ISet.singleton(i)).toMap
      }
    }
  }

  def hasConflicts(path: Path, idx: Int): Boolean = {
    val hasOverrides = for {
      matches <- conflicts.get(path)
      max <- matches.maximum
    } yield idx < max

    hasOverrides | false
  }

  def recoveryIndex(path: Path, idx: Int): Option[Int] =
    for {
      matches <- conflicts.get(path)
      value <- matches.lookupLT(idx)
    } yield value
}
