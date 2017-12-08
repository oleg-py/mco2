package mco.core.state

import scala.collection.immutable.SortedSet

import cats._
import cats.implicits._
import mouse.all._
import mco.core.paths._
import monocle.macros.Lenses


@Lenses case class RepoState (
  orderedMods: Vector[Pointed[ModState]] = Vector(),
  labels: Map[RelPath, String] = Map()
) {
  lazy val conflicts: Map[Path, SortedSet[Int]] = {
    orderedMods.zipWithIndex.foldMap { case (Pointed(_, mod), i) =>
      mod.stamp.enabled ?? mod.contents.values.toList.foldMap { cState =>
        cState.stamp.enabled ?? cState.target.tupleRight(SortedSet(i)).toMap
      }
    }
  }

  def overrideIndex(path: Path, idx: Int): Option[Int] =
    conflicts.get(path) >>= (_.from(idx).drop(1).lastOption)

  def recoveryIndex(path: Path, idx: Int): Option[Int] =
    conflicts.get(path) >>= (_.until(idx).lastOption)

  def hasConflicts(path: Path, idx: Int): Boolean =
    overrideIndex(path, idx).cata(idx < _, false)

  def at(key: RelPath): (Int, Pointed[ModState]) =
    orderedMods.zipWithIndex.find(_._1.key == key)
      .getOrElse(sys.error(s"Invariant violation at $key"))
      .swap

  def add(mod: Pointed[ModState], label: String): RepoState =
    copy(orderedMods :+ mod, labels.updated(mod.key, label))

  def remove(key: RelPath): RepoState =
    copy(orderedMods = orderedMods.filter(_.key != key))
}
