package mco.io

import cats._
import cats.implicits._

import mco.core.paths.Path
import mco.core.{Content, Mod}
import mco.core.state.{ContentState, ModState, Stamp}

object state {
  def initContent[F[_]: Filesystem: Monad](c: Content)(p: Path): F[ContentState] = {
    Filesystem.hashAt(p) map { hash =>
      val enabled = c match {
        case Content.Component | Content.Document => true
        case _ => false
      }
      ContentState(Stamp(enabled))
    }
  }

  def initMod[F[_]: Filesystem: Monad](mod: Mod[F]): F[ModState] =
    for {
      provided <- mod.filterProvide(_ => true)
      inner <- provided
        .andThen { vec =>
          vec.traverse { la => initContent(Content.Component)(la.get).tupleLeft(la.key)}
        }
        .runFS
    } yield ModState(Stamp(enabled = false), inner.toMap)
}
