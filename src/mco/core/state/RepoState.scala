package mco.core.state

import scalaz._
import std.vector._
import std.map._
import syntax.monoid._

import mco.util.syntax.fp._
import mco.data.{MinMax, Path}


case class RepoState (
  orderedMods: Vector[ModState]
) {
  lazy val conflicts: Map[Path, MinMax] = {
    orderedMods.indexed.foldMap { case (i, mod) =>
      mod.contents.foldMap { contentState =>
        contentState.target.strengthR(MinMax(i, i)).toMap
      }
    }
  }

  def hasConflicts(path: Path, idx: Int) =
    conflicts.getOrElse(path, mzero[MinMax]).max > idx
}
