package mco.io.generic

import scalaz._
import std.option._
import std.stream._
import syntax.std.map._

import mco.core.ImageStore
import mco.core.vars.Var
import mco.data.{Key, Path}
import mco.util.syntax.any._
import mco.util.syntax.fp._
import mco.util.misc.{nextFreeName, strHashes}
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
  root: Path,
  store: Var[F, Map[Key, String]]
) extends ImageStore[F] {
  private def mkName(path: Path) =
    nextFreeName[F](root, path.extension)(strHashes(path.asString))

  override def getImage(key: Key): F[Option[Path]] =
    for {
      dict <- store()
    } yield dict.get(key).map(root / _)

  override def putImage(key: Key, path: Option[Path]): F[Unit] =
    for {
      dict <- store()
      newName <- path.traverse(mkName)
      named = path.tuple(newName)
      _ <- dict.get(key).cata(name => rmTree(root / name), unit.point[F])
      _ <- named.fold(unit.point[F]) { case (src, target) =>
        move(src, target)
      }
      _ <- store ~= { _.alter(key)(_ => newName.map(_.relStringTo(root))) }
    } yield ()

  override def stripImages(keys: Vector[Key]): F[Unit] =
    for {
      dict <- store()
      keySet = keys.toSet
      (oks, dels) = dict.partition { case (k, _) => keySet(k) }
      _ <- store := oks
      _ <- dels.valuesIterator.toStream
        .traverse { name => rmTree(root / name) }
    } yield ()
}
