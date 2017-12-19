package mco.core.state

import mco.core.Status
import mco.core.paths._
import monocle.Lens
import monocle.macros.Lenses


@Lenses case class RepoState (
  orderedMods: Vector[Pointed[ModState]] = Vector(),
  labels: Map[RelPath, String] = Map()
) {
  def at(key: RelPath): (Int, Pointed[ModState]) =
    orderedMods.zipWithIndex.find(_._1.key == key)
      .getOrElse(sys.error(s"Invariant violation at $key"))
      .swap

  def add(mod: Pointed[ModState], label: String): RepoState =
    copy(orderedMods :+ mod, labels.updated(mod.key, label))

  def remove(key: RelPath): RepoState =
    copy(orderedMods.filter(_.key != key), labels - key)

  def tagInstall(key: RelPath, filter: RelPath => Boolean, status: Status) =
    copy(orderedMods map {
      case Pointed(`key`, modState) =>
        Pointed(key, modState.tagInstall(filter, status))
      case a => a
    })
}

object RepoState {
  def pathL(p: RelPath): Lens[RepoState, ModState] =
    Lens[RepoState, ModState](_.at(p)._2.get)(newState =>
      RepoState.orderedMods.modify(_.map {
        case Pointed(`p`, _) => Pointed(p, newState)
        case a => a
      }))
}