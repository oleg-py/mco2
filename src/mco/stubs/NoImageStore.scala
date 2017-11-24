package mco.stubs

import scalaz._
import std.option._
import syntax.applicative._

import mco.core.ImageStore
import mco.data.Key
import mco.data.paths.Path
import mco.util.syntax.any._

import java.net.URL


/**
 * Image store that has no association for any key and does nothing
 *
 * @tparam F context of result values
 */
class NoImageStore[F[_]: Applicative] extends ImageStore[F] {
  override def getImage(key: Key) = none[URL].point[F]
  override def putImage(key: Key, path: Option[Path]) = unit.point[F]
  override def stripImages(keys: Vector[Key]) = unit.point[F]
}
