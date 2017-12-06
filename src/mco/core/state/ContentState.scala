package mco.core.state

import mco.core.paths.Path
import monocle.macros.Lenses


@Lenses case class ContentState(
  stamp: Stamp,
  target: Option[Path] = None
) {
  def onResolve(target: Path, installed: Boolean): ContentState = copy(
    stamp = stamp.copy(installed = installed),
    target = Some(target)
  )
}
