package mco.core.state

import scalaz._
import std.vector._
import std.anyVal._
import std.map._

import mco.data.Path
import mco.util.syntax.fp._


case class RepoState (
  orderedMods: Vector[ModState]
) {
  lazy val conflicts: Map[Path, ISet[Int]] = {
    orderedMods.indexed.foldMap { case (i, mod) =>
      if (mod.stamp.enabled) {
        mod.contents.foldMap { contentState =>
          contentState.target.strengthR(ISet.singleton(i)).toMap
        }
      } else {
        Map.empty[Path, ISet[Int]]
      }
    }
  }

  def hasConflicts(path: Path, idx: Int) = {
    val hasOverrides = for {
      matches <- conflicts.get(path)
      max <- matches.maximum
    } yield idx < max

    hasOverrides | false
  }
}
