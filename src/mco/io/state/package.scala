package mco.io

import scalaz._
import std.vector._

import mco.core._
import mco.core.state._
import mco.data.{Path, TempOp}
import mco.io.generic.Filesystem
import mco.util.syntax.any._
import mco.util.syntax.fp._

package object state {
  def initContent[F[_]: Filesystem: Monad](c: Content.Plain)(p: Path): F[ContentState] = {
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
      data <- mod.filterProvide(_.get == Content.Component).runFS
      inner <- data
        .map(la => (la.key, la.get))
        .pipe(IMap.fromFoldable(_))
        .traverse(initContent[F](Content.Component))
    } yield ModState(inner.foldMap(_.stamp).copy(
      enabled = false,
      installed = false
    ), inner)
}
