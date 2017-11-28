package mco.io

import scalaz._
import std.vector._

import mco.core._
import mco.core.state._
import mco.data.paths.Path
import mco.io.generic.Filesystem
import mco.util.syntax.any._
import mco.util.syntax.fp._

package object state {
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
