package mco.ui

case class UiState(error: Option[Throwable]) {
  def clearError = copy(error = None)
}
