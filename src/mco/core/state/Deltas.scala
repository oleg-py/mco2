package mco.core.state

import mco.core.ContentKind
import mco.core.paths.RelPath
import mco.syntax._
import monocle.macros.syntax.lens._

object Deltas {
  private def setIfPresent[A](o: Option[A])(a: A): A = o.getOrElse(a)

  case class OfContent(
    enabled: Option[Boolean] = None,
    assignedKind: Option[ContentKind] = None
  ) {
    def patch(cs: ContentState): ContentState = {
      cs.lens(_.stamp.enabled).modify(setIfPresent(enabled))
        .lens(_.assignedKind).modify(setIfPresent(assignedKind))
    }
  }

  case class OfMod(
    enabled: Option[Boolean] = None,
    contents: Map[RelPath, OfContent] = Map()
  ) {
    def patch(ms: ModState): ModState = {
      ms.lens(_.stamp.enabled).modify(setIfPresent(enabled))
        .lens(_.contents).modify { map =>
        contents.foldLeft(map) { case (updated, (key, delta)) =>
          updated.adjust(key, delta.patch)
        }
      }
    }
  }
}
