package mco.io

import cats._
import cats.implicits._

import com.olegpy.forwarders
import mco.core.paths.Path

import java.net.URL
import java.nio.file.attribute.BasicFileAttributes


/**
 * Typeclass expressing algebra of filesystem operations
 *
 * @tparam F the effect type
 */
@forwarders trait Filesystem[F[_]] {
  // --- Contents/creation of the file ---
  def getBytes(path: Path): F[Array[Byte]]
  def setBytes(path: Path, cnt: Array[Byte]): F[Unit]

  // --- Contents/creation of the dir ---
  def childrenOf(path: Path): F[Stream[Path]]
  def mkDir(path: Path): F[Unit]
  def rmTree(path: Path): F[Unit]

  // --- Moving/deleting of files and folders ---
  def copy(source: Path, dest: Path): F[Unit]
  def move(source: Path, dest: Path): F[Unit]

  def stat(path: Path): F[Option[BasicFileAttributes]]

  // --- Special operations for concrete use cases ---
  def archiving: Archiving[F]
  def runTmp[A](f: Path => F[A]): F[A]
  def fileToUrl(p: Path): F[URL]
  protected[mco] def hashFile(p: Path): F[(Long, Long)]

  // --- Derived operations ---
  final def hashAt(p: Path)(implicit M: Monad[F]): F[(Long, Long)] = {
    def dirHash = childrenOf(p) >>= { stream => stream foldMapM hashAt }
    def getHash = isDirectory(p).ifM(dirHash, hashFile(p))
    exists(p).ifM(getHash, (0L, 0L).pure[F])
  }

  final def exists(path: Path)(implicit F: Functor[F]): F[Boolean] =
    stat(path).map(_.isDefined)

  final def isRegularFile(path: Path)(implicit F: Functor[F]) : F[Boolean] =
    stat(path).map(_.fold(false)(_.isRegularFile))

  final def isDirectory(path: Path)(implicit F: Functor[F])   : F[Boolean] =
    stat(path).map(_.fold(false)(_.isDirectory))

  final def ensureDir(path: Path)(implicit F: Monad[F]): F[Unit] =
    isDirectory(path).ifM(F.pure(()), mkDir(path))

  final def rmIfExists(path: Path)(implicit F: Monad[F]): F[Unit] =
    exists(path).ifM(rmTree(path), F.pure(()))
}


