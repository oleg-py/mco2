package mco.core.state

import mco.core.{ContentKind, Status}
import mco.core.paths.RelPath
import mco.syntax._
import monocle.macros.syntax.lens._

object Deltas {
  private def setIfPresent[A](o: Option[A])(a: A): A = o.getOrElse(a)

  case class OfContent(
    status: Option[Status] = None,
    assignedKind: Option[ContentKind] = None
  ) {
    def patch(cs: ContentState): ContentState = {
      cs.lens(_.status).modify(setIfPresent(status))
        .lens(_.assignedKind).modify(setIfPresent(assignedKind))
    }
  }

  case class OfMod(
    status: Option[Status] = None,
    contents: Map[RelPath, OfContent] = Map()
  ) {
    def patch(ms: ModState): ModState = {
      ms.lens(_.status).modify(setIfPresent(status))
        .lens(_.contents).modify { map =>
        contents.foldLeft(map) { case (updated, (key, delta)) =>
          updated.adjust(key, delta.patch)
        }
      }
    }
  }
}
