package mco.io.generic

import java.nio.file.attribute.BasicFileAttributes
import scalaz._
import std.stream._
import syntax.equal._

import mco.data.paths._
import mco.util.syntax.fp._


class VirtualizedFilesystem[F[_]: Monad](
  roots: Map[Segment, (Path, Filesystem[F])],
  runTmpFs: Filesystem[F]
)(
  implicit val fsEq: Equal[Filesystem[F]]
) extends Filesystem[F] {
  private def retranslate(path: Path): (Path, Filesystem[F], Segment) = {
    path.segments match {
      case Vector() => (Path.root, this, Segment.empty)
      case head +: rest =>
        val (root, fs) = roots(head)
        (root / RelPath(rest), fs, head)
    }
  }

  private def unaryOp[A](path: Path)(op: (Filesystem[F], Path) => A): A = {
    val (p, fs, _) = retranslate(path)
    op(fs, p)
  }

  override def childrenOf(path: Path): F[Stream[Path]] = {
    val (p, fs, head) = retranslate(path)
    if (fs === this) {
      roots.keys.toStream.map(Path.root / _).point[F]
    } else {
      fs.childrenOf(p)
        .map { paths => paths.map(Path.root / head / _.fromToP(p)) }
    }
  }

  override def getBytes(path: Path): F[ImmutableArray[Byte]] =
    unaryOp(path) { _ getBytes _ }

  override def setBytes(path: Path, cnt: ImmutableArray[Byte]): F[Unit] =
    unaryOp(path) { _ setBytes (_, cnt) }

  override def mkDir(path: Path): F[Unit] =
    unaryOp(path) { _ mkDir _ }

  private def slowCopy(from: Path, to: Path): F[Unit] = {
    def fileCopy = getBytes(from) >>= { setBytes(to, _) }
    def folderCopy = for {
      children <- childrenOf(from)
      _ <- children.traverse_ { path =>
        slowCopy(path, to / path.fromToP(from))
      }
    } yield ()

    isRegularFile(from).ifM(fileCopy, folderCopy)
  }

  override def copy(source: Path, dest: Path): F[Unit] = {
    val (p1, fs1, _) = retranslate(source)
    val (p2, fs2, _) = retranslate(dest)
    if (fs1 === fs2) fs1.copy(p1, p2)
    else slowCopy(source, dest)
  }

  override def move(source: Path, dest: Path): F[Unit] = {
    val (p1, fs1, _) = retranslate(source)
    val (p2, fs2, _) = retranslate(dest)
    if (fs1 === fs2) fs1.move(p1, p2)
    else slowCopy(source, dest) >> rmTree(source)
  }

  override def rmTree(path: Path): F[Unit] =
    unaryOp(path) { _ rmTree _ }

  override def stat(path: Path): F[Option[BasicFileAttributes]] =
    unaryOp(path) { _ stat _ }

  override def runTmp[A](f: Path => F[A]): F[A] =
    runTmpFs.runTmp(f)

  override protected[mco] def hashFile(path: Path): F[(Long, Long)] =
    unaryOp(path) { _ hashFile _ }

  override def fileToUrl(p: Path) =
    unaryOp(p) { _ fileToUrl _ }
}
