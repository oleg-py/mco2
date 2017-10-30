package mco.io

import scalaz._
import std.vector._

import mco.core._
import mco.core.state._
import mco.data.Path
import mco.io.generic.Filesystem
import mco.io.state.Hashing._
import mco.util.syntax.any._
import mco.util.syntax.fp._

package object state {
  def initContent[F[_]: Hashing: Filesystem: Monad](c: Content.Plain)(p: Path): F[ContentState] = {
    hashAt(p) map { hash =>
      val enabled = c match {
        case Content.Component | Content.Document => true
        case _ => false
      }
      ContentState(Stamp(hash, enabled))
    }
  }

  def initMod[F[_]: Filesystem: Hashing: Monad](mod: Mod[F]): Path.Temp[F, ModState] = tmp =>
    for {
      data <- mod.provideChildren(Content.Component).apply(tmp)
      inner <- data
        .map(la => (la.key, la.get))
        .pipe(IMap.fromFoldable(_))
        .traverse(initContent[F](Content.Component))
    } yield ModState(inner.foldMap(_.stamp), inner)
}
