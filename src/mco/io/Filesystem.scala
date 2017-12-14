package mco.io

import cats._
import cats.implicits._
import com.olegpy.forwarders
import mco.core.paths.Path
import net.sf.sevenzipjbinding.{IInStream, IOutStream}

import java.net.URL
import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes


/**
 * Algebra of filesystem operations
 *
 * @tparam F the effect type
 */
@forwarders trait Filesystem[F[_]] {
  // --- Contents/creation of the file ---
  def readFile(path: Path): fs2.Stream[F, ByteBuffer]
  def writeFile(path: Path, bb: ByteBuffer): F[Unit]

  // --- Contents/creation of the dir ---
  def childrenOf(path: Path): F[Stream[Path]]
  def mkDir(path: Path): F[Unit]
  def rmTree(path: Path): F[Unit]

  // --- Moving/deleting of files and folders ---
  def copy(source: Path, dest: Path): F[Unit]
  def move(source: Path, dest: Path): F[Unit]

  def stat(path: Path): F[Option[BasicFileAttributes]]

  // --- Special operations for concrete use cases ---
  def fileToUrl(p: Path): F[URL]
  def mkTemp: fs2.Stream[F, Path]
  def getSfStream(path: Path): fs2.Stream[F, IInStream with IOutStream]

  // --- Derived operations ---
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


