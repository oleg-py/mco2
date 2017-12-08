package mco.game.generic.store

import cats._
import cats.implicits._
import mouse.all._

import mco.core.ImageStore
import mco.core.paths._
import mco.core.vars.Var
import mco.io.Filesystem, Filesystem._
import mco.util.syntax.any._
import mco.util.syntax.map._

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
  store: Var[F, Map[RelPath, RelPath]],
  isImage: Segment => Boolean
) extends ImageStore[F] {
  private def getTarget(key: RelPath)(path: Path) =
    rel"${key.name}${path.extension}"

  private val noop = unit.pure[F]

  override def getImage(key: RelPath): F[Option[URL]] =
    for {
      dict <- store()
      path = dict.get(key).map(root / _)
      exists <- path.traverse(exists(_)).map(_ getOrElse false)
      url <- path.filter(_ => exists).traverse(fileToUrl(_))
    } yield url

  override def putImage(key: RelPath, path: Option[Path]): F[Unit] = {
    def copyFile = for {
      dict <- store()
      newName = path.map(getTarget(key))
      named = (path, newName.map(root / _)).tupled
      _ <- dict.get(key).cata(name => rmIfExists(root / name), noop)
      _ <- named.cata(Function.tupled(copy(_, _)), noop)
      _ <- store ~= { _.alter(key, _ => newName) }
    } yield ()

    if (path.forall(p => isImage(p.name))) copyFile
    else noop
  }

  override def stripImages(keys: Vector[RelPath]): F[Unit] =
    for {
      dict <- store()
      keySet = keys.toSet
      (oks, dels) = dict.partition { case (k, _) => keySet(k) }
      _ <- store := oks
      _ <- dels.values.toList.traverse { name => rmIfExists(root / name) }
    } yield ()
}
