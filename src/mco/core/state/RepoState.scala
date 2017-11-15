package mco.core.state

import scalaz._
import std.vector._
import std.option._
import std.anyVal._
import std.map._
import syntax.std.boolean._

import mco.data.{Key, Keyed, Path}
import mco.util.syntax.fp._
import monocle.macros.Lenses


@Lenses case class RepoState (
  orderedMods: Vector[Keyed[ModState]],
  labels: Map[Key, String]
) {
  lazy val conflicts: Map[Path, ISet[Int]] = {
    orderedMods.indexed.foldMap { case (i, Keyed(_, mod)) =>
      mod.stamp.enabled ?? mod.contents.foldMap { cState =>
        cState.stamp.enabled ?? cState.target.strengthR(ISet.singleton(i)).toMap
      }
    }
  }

  def overrideIndex(path: Path, idx: Int): Option[Int] =
    conflicts.get(path) >>= (_.lookupGT(idx))

  def recoveryIndex(path: Path, idx: Int): Option[Int] =
    conflicts.get(path) >>= (_.lookupLT(idx))

  def hasConflicts(path: Path, idx: Int): Boolean =
    overrideIndex(path, idx).cata(idx < _, false)

  def at(key: Key): (Int, Keyed[ModState]) =
    orderedMods.indexed.find(_._2.key == key)
      .err(s"Invariant violation at $key")

  def add(mod: Keyed[ModState], label: String): RepoState =
    copy(orderedMods :+ mod, labels.updated(mod.key, label))

  def remove(key: Key): RepoState =
    copy(orderedMods = orderedMods.filter(_.key != key))
}
