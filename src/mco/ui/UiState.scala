package mco.ui

import mco.core.state.{ModState, RepoState}
import mco.ui.UiState.PendingAdds

import java.net.URL


case class UiState(
  repoState: RepoState = RepoState(),
  pendingAdds: Option[PendingAdds] = None,
  currentMod: Option[ModState] = None,
  thumbnailUrl: Option[URL] = None,
  error: Option[Throwable] = None
) {
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
