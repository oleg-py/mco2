package mco.core.state

import mco.core.paths.RelPath
import monocle.function.Index.index


object Deltas {
  case class OfContent(
    enabled: Option[Boolean] = None
  ) {
    def patch(c: ContentState): ContentState = {
      enabled.fold(c)(bool => ContentState.stamp.modify(_.copy(enabled = bool))(c))
    }
  }

  case class OfMod(
    label: Option[String] = None,
    enabled: Option[Boolean] = None,
    contents: Map[RelPath, OfContent] = Map()
  ) {
    def patch(m: ModState): ModState = {
      val installedSet =
        enabled.fold(m)(bool => ModState.stamp.modify(_.copy(enabled = bool))(m))

      contents.foldLeft(installedSet) { case (modState, (key, delta)) =>
        val change = ModState.contents composeOptional index(key) modify (delta patch _)
        change(modState)
      }
    }
  }
}
