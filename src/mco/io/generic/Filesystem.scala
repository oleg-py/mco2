package mco.io.generic

import scalaz._
import scalaz.syntax.functor._

import com.olegpy.forwarders
import mco.data.Path

import java.nio.file.attribute.BasicFileAttributes


@forwarders trait Filesystem[F[_]] {
  def childrenOf(path: Path)                 : F[Stream[Path]]
  def getBytes(path: Path)                   : F[ImmutableArray[Byte]]
  def setBytes(path: Path, cnt: ImmutableArray[Byte]) : F[Unit]
  def mkDir(path: Path)                      : F[Unit]
  def copy(source: Path, dest: Path): F[Unit]
  def move(source: Path, dest: Path): F[Unit]
  def rmTree(path: Path)                     : F[Unit]

  def stat(path: Path): F[Option[BasicFileAttributes]]

  final def exists(path: Path)(implicit F: Functor[F]): F[Boolean] = {
    stat(path).map(_.isDefined)
  }

  final def isRegularFile(path: Path)(implicit F: Functor[F]) : F[Boolean] =
    stat(path).map(_.fold(false)(_.isRegularFile))

  final def isDirectory(path: Path)(implicit F: Functor[F])   : F[Boolean] =
    stat(path).map(_.fold(false)(_.isDirectory))

  final def ensureDir(path: Path)(implicit F: Monad[F]): F[Unit] =
    F.ifM(isDirectory(path), F.point(()), mkDir(path))
}


