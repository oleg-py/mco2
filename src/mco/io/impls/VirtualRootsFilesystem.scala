package mco.io.impls

import java.nio.file.attribute.BasicFileAttributes
import cats._
import cats.effect.Sync
import cats.syntax.all._
import cats.instances.stream._
import mco.core.paths._
import mco.io.Filesystem
import net.sf.sevenzipjbinding.{IInStream, IOutStream}

import java.nio.ByteBuffer


class VirtualRootsFilesystem[F[_]: Sync](
  roots: Map[Segment, (Path, Filesystem[F])],
  runTmpRoot: Segment,
  runTmpFs: Filesystem[F]
)(
  implicit val fsEq: Eq[Filesystem[F]]
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

  override def readFile(path: Path): fs2.Stream[F, ByteBuffer] =
    unaryOp(path)(_.readFile(_))

  override def writeFile(path: Path, bb: ByteBuffer): F[Unit] =
    unaryOp(path)(_.writeFile(_, bb))

  override def mkTemp: fs2.Stream[F, Path] =
    runTmpFs.mkTemp.map(p => path"$runTmpRoot/$p")

  override def getSfStream(path: Path): fs2.Stream[F, IInStream with IOutStream] =
    unaryOp(path)(_.getSfStream(_))

  override def childrenOf(path: Path): F[Stream[Path]] = {
    val (p, fs, head) = retranslate(path)
    if (fs === this) {
      roots.keys.toStream.map(Path.root / _).pure[F]
    } else {
      fs.childrenOf(p)
        .map { paths => paths.map(Path.root / head / _.relTo(p)) }
    }
  }

  override def mkDir(path: Path): F[Unit] =
    unaryOp(path) { _ mkDir _ }

  private def slowCopy(from: Path, to: Path): F[Unit] = {
    def fileCopy = readFile(from).evalMap(writeFile(to, _)).runSync
    def folderCopy = for {
      children <- childrenOf(from)
      _ <- children.traverse_ { path =>
        slowCopy(path, to / path.relTo(from))
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
    else slowCopy(source, dest) *> rmTree(source)
  }

  override def rmTree(path: Path): F[Unit] =
    unaryOp(path) { _ rmTree _ }

  override def stat(path: Path): F[Option[BasicFileAttributes]] =
    unaryOp(path) { _ stat _ }

  override def fileToUrl(p: Path) =
    unaryOp(p) { _ fileToUrl _ }
}
