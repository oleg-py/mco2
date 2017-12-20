package mco.stubs

import cats._
import cats.syntax.option._
import cats.syntax.applicative._

import mco.core.ImageStore
import mco.core.paths._
import mco.syntax._

import java.net.URL


/**
 * Image store that has no association for any key and does nothing
 */
class NoImageStore[F[_]: Applicative] extends ImageStore[F] {
  override def getImage(key: RelPath) = none[URL].pure[F]
  override def putImage(key: RelPath, path: Option[Path]) = ().pure[F]
}
