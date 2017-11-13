package mco.core

import mco.core.state.{ContentState, ModState}
import mco.data.Key
import monocle.function.Index.index


object Deltas {
  case class OfContent(
    enabled: Option[Boolean] = None
  ) {
    def patch(c: ContentState): ContentState = {
      enabled.fold(c)(bool => ContentState.stamp.modify(_.copy(installed = bool))(c))
    }
  }

  case class OfMod(
    installed: Option[Boolean] = None,
    contents: Map[Key, OfContent] = Map()
  ) {
    def patch(m: ModState): ModState = {
      val installedSet =
        installed.fold(m)(bool => ModState.stamp.modify(_.copy(installed = bool))(m))

      contents.foldLeft(installedSet) { case (modState, (key, delta)) =>
        val change = ModState.contents composeOptional index(key) modify (delta patch _)
        change(modState)
      }
    }
  }
}
