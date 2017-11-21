package mco.ui

import mco.core.state.{ContentState, ModState, RepoState}
import mco.data.Key
import mco.ui.UiState.PendingAdds
import java.net.URL

import scalaz.std.string._

import monocle.{Optional, POptional}
import monocle.macros.Lenses
import monocle.std.option.{some => someL}

@Lenses case class UiState(
  repoState: RepoState = RepoState(),
  pendingAdds: Option[PendingAdds] = None,
  currentModKey: Option[Key] = None,
  thumbnailUrl: Option[URL] = None,
  error: Option[Throwable] = None
) {
  def currentMod: Option[ModState] = for {
    key <- currentModKey
    mod <- repoState.orderedMods.find(_.key == key)
  } yield mod.get

  def currentContent: Vector[(String, ContentState)] =
    currentMod
      .map(_.contents.mapKeys(_.unwrap).toList.toVector)
      .getOrElse(Vector())
}

object UiState {
  val assocL: Optional[UiState, Map[String, Option[String]]] =
    UiState.pendingAdds
      .composePrism(someL)
      .composeLens(PendingAdds.assoc)

  @Lenses case class PendingAdds (
    packages: Vector[String] = Vector(),
    images: Vector[String] = Vector(),
    assoc: Map[String, Option[String]] = Map()
  )

  def startupError(throwable: Throwable) =
    UiState(error = Some(throwable))

  def initial(repoState: RepoState) =
    UiState(repoState)
}
