package mco.ui

import mco.core.state.RepoState
import mco.data.Key
import mco.ui.UiState.PendingAdds

import java.net.URL
import scalaz.std.string._

case class UiState(
  repoState: RepoState = RepoState(),
  pendingAdds: Option[PendingAdds] = None,
  currentModKey: Option[Key] = None,
  thumbnailUrl: Option[URL] = None,
  error: Option[Throwable] = None
) {
  def currentMod = for {
    key <- currentModKey
    mod <- repoState.orderedMods.find(_.key == key)
  } yield mod.get

  def currentContent =
    currentMod
      .map(_.contents.mapKeys(_.unwrap).toList.toVector)
      .getOrElse(Vector())

  def clearError = copy(error = None)
}

object UiState {
  case class PendingAdds (
    packages: Vector[String] = Vector(),
    images: Vector[String] = Vector(),
    assoc: Map[String, Option[String]] = Map()
  )

  def startupError(throwable: Throwable) =
    UiState(error = Some(throwable))

  def initial(repoState: RepoState) =
    UiState(repoState)
}
