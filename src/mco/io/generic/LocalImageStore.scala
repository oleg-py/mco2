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
import Filesystem._

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
  store: Var[F, Map[Key, String]]
) extends ImageStore[F] {
  private def getTarget(key: Key)(path: Path) =
    Path.segment(key.unwrap) ++ path.extension

  private val noop = unit.point[F]

  override def getImage(key: Key): F[Option[URL]] =
    for {
      dict <- store()
      filename = dict.get(key)
      url <- filename.traverse(s => fileToUrl(root / s))
    } yield url

  override def putImage(key: Key, path: Option[Path]): F[Unit] =
    for {
      dict <- store()
      newName = path.map(getTarget(key))
      named = path.tuple(newName.map(root / _))
      _ <- dict.get(key).cata(name => rmTree(root / name), noop)
      _ <- named.cata(Function.tupled(copy(_, _)), noop)
      _ <- store ~= { _.alter(key)(_ => newName) }
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
