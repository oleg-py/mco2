package mco.core.state

import mco.core.{ContentKind, Status}
import mco.core.paths.Path
import monocle.macros.Lenses


@Lenses case class ContentState(
  status: Status,
  assignedKind: ContentKind,
  target: Option[Path] = None
) {
  def tagInstall(status: Status): ContentState = copy(status)
}
