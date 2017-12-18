package mco.core.state

import mco.core.Status
import mco.core.paths._
import mco.syntax._
import monocle.macros.Lenses


@Lenses case class ModState(
  status: Status,
  contents: Map[RelPath, ContentState]
) {
  def tagInstall(filter: RelPath => Boolean, status: Status): ModState =
    copy(
      status,
      contents map {
        case (key, value) if filter(key) => (key, value.tagInstall(status))
        case a => a
      }
    )

}
