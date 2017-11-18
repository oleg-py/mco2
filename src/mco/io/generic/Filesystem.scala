package mco.io.generic

import scalaz._
import std.stream._
import std.anyVal._
import std.tuple._

import com.olegpy.forwarders
import mco.data.Path
import mco.util.syntax.fp._

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

  def runTmp[A](f: Path => F[A]): F[A]

  protected def hashFile(p: Path): F[(Long, Long)]

  final def hashAt(p: Path)(implicit F: Filesystem[F], M: Monad[F]): F[(Long, Long)] = {
    def dirHash = childrenOf(p) >>= { s => s.foldMapM(hashAt) }
    def getHash = isDirectory(p).ifM(dirHash, hashFile(p))
    exists(p).ifM(getHash, (0L, 0L).point[F])
  }

  final def exists(path: Path)(implicit F: Functor[F]): F[Boolean] = {
    stat(path).map(_.isDefined)
  }

  final def isRegularFile(path: Path)(implicit F: Functor[F]) : F[Boolean] =
    stat(path).map(_.fold(false)(_.isRegularFile))

  final def isDirectory(path: Path)(implicit F: Functor[F])   : F[Boolean] =
    stat(path).map(_.fold(false)(_.isDirectory))

  final def ensureDir(path: Path)(implicit F: Monad[F]): F[Unit] =
    isDirectory(path).ifM(F.point(()), mkDir(path))
}


