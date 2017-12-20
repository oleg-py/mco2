package mco.game.generic.store

import cats._
import cats.implicits._

import mco.core.ImageStore
import mco.core.paths._
import mco.io.Filesystem, Filesystem._
import mco.syntax._

import java.net.URL


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
  root: Path,
  imgExtensions: Vector[String]
) extends ImageStore[F]
{

  private def possibleImgNames(target: Path) =
    imgExtensions.map(target.withExtension) ++
      imgExtensions.map(ext => target / seg".." / seg"${target.name}$ext")

  override def getImage(key: RelPath): F[Option[URL]] = {
    possibleImgNames(root / key)
      .traverse(toURLIfExists)
      .map(_.foldK)
  }

  override def putImage(key: RelPath, path: Option[Path]): F[Unit] = {
    val target = root / key
    path match {
      case Some(image) =>
        val imgExt = image.extension
        ifM (imgExtensions.contains(imgExt)) {
          copy(image, target.withExtension(imgExt))
        }
      case None =>
        possibleImgNames(target).traverse_(rmIfExists(_))
    }
  }

  private def toURLIfExists(path: Path) =
    exists(path).ifM(
      fileToUrl(path).map(_.some),
      none[URL].pure[F]
    )
}
