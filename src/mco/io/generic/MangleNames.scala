package mco.io.generic

import scalaz._
import std.anyVal._
import std.option._
import std.tuple._

import mco.core._
import mco.core.state.ModState
import mco.data.{Key, Keyed, Path}
import mco.io.generic.Filesystem._
import mco.util.syntax.fp._

import java.util.UUID

class MangleNames[F[_]: Filesystem: Monad](target: Path)
  extends NameResolver[F]
{
  private val uuidName = new UUID(_: Long, _: Long).toString
  private val step = (1L, 3L)
  override def apply(mod: Keyed[ModState])(content: Key): F[Option[Path]] = {
    // TODO - extract ext. without dummy path
    val ext = (Path.root / content.unwrap).extension
    val toName = uuidName.tupled.andThen(_ ++ ext)

    def nextFreeName(hash: (Long, Long)): F[Path] = {
      val candidate = target / toName(hash)
      exists(candidate).ifM(
        nextFreeName(hash |+| step),
        candidate.point[F]
      )
    }

    mod.get.contents.lookup(content)
      .map(_.stamp.hash)
      .traverse(nextFreeName)
  }
}
