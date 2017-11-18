package mco.io.generic

import scalaz.Monad
import scalaz.std.stream._

import mco.core.ImageStore
import mco.data.{Key, Path}
import mco.util.syntax.fp._
import Filesystem._


/**
 * Image store based on separate folder
 * It moves files inside to create association and
 * removes them correspondingly
 *
 * @param root directory image files are stored in
 * @tparam F context of result values
 */
//noinspection ConvertibleToMethodValue
class LocalImageStore[F[_]: Filesystem: Monad](
  root: Path
) extends ImageStore[F] {
  // TODO - png / jpg separation
  private def toPath(key: Key) =
    root / (Path(key.unwrap).segments.mkString("$slash$") ++ ".png")

  override def getImage(key: Key) = {
    val target = toPath(key)
    for (children <- childrenOf(root)) yield
      children.find(_ == target)
  }

  override def putImage(key: Key, path: Option[Path]) =
    path.cata(copy(_, toPath(key)), rmTree(toPath(key)))

  override def stripImages(keys: Vector[Key]) = {
    childrenOf(root) >>= { _
      .filterNot(keys.map(toPath).toSet)
      .traverse_(rmTree(_))
    }
  }
}
