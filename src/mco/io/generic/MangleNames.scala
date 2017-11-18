package mco.io.generic

import scalaz._
import std.option._

import mco.core._
import mco.core.state.ModState
import mco.data.{Key, Keyed, Path}
import mco.util.syntax.fp._
import mco.util.misc.nextFreeName

class MangleNames[F[_]: Filesystem: Monad](target: Path)
  extends NameResolver[F]
{
  private val step = (1L, 3L)
  override def apply(mod: Keyed[ModState])(content: Key): F[Option[Path]] = {
    // TODO - extract ext. without dummy path
    val ext = (Path.root / content.unwrap).extension

    mod.get.contents.lookup(content)
      .map(_.stamp.hash)
      .traverse(nextFreeName[F](target, ext))
  }
}
