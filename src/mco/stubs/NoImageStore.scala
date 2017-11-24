package mco.stubs

import scalaz._
import std.option._
import syntax.applicative._

import mco.core.ImageStore
import mco.data.paths.{Path, RelPath}
import mco.util.syntax.any._

import java.net.URL


/**
 * Image store that has no association for any key and does nothing
 *
 * @tparam F context of result values
 */
class NoImageStore[F[_]: Applicative] extends ImageStore[F] {
  override def getImage(key: RelPath) = none[URL].point[F]
  override def putImage(key: RelPath, path: Option[Path]) = unit.point[F]
  override def stripImages(keys: Vector[RelPath]) = unit.point[F]
}
