package mco.io

import scalaz._
import std.vector._

import mco.core.paths.Path
import mco.core.{Content, Mod}
import mco.core.state.{ContentState, ModState, Stamp}
import mco.util.syntax.fp._

object state {
  def initContent[F[_]: Filesystem: Monad](c: Content)(p: Path): F[ContentState] = {
    Filesystem.hashAt(p) map { hash =>
      val enabled = c match {
        case Content.Component | Content.Document => true
        case _ => false
      }
      ContentState(Stamp(hash, enabled))
    }
  }

  def initMod[F[_]: Filesystem: Monad](mod: Mod[F]): F[ModState] =
    for {
      provided <- mod.filterProvide(_ => true)
      inner <- provided
        .andThen { vec =>
          IMap.fromFoldable(vec.map(la => (la.key, la.get)))
            .traverse(initContent[F](Content.Component))
        }
        .runFS
    } yield ModState(inner.foldMap(_.stamp).copy(
      enabled = false,
      installed = false
    ), inner)
}
