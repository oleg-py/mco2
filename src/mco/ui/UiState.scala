package mco.ui

import mco.core.state.RepoState
import mco.ui.UiState.PendingAdds


case class UiState(
  pendingAdds: Option[PendingAdds],
  error: Option[Throwable]
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
    UiState(None, Some(throwable))

  def initial(repoState: RepoState) =
    UiState(None, None)
}
