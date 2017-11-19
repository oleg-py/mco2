package mco.io.generic

import scalaz._
import std.stream._
import syntax.equal._
import mco.util.syntax.fp._

import mco.data.Path


class VirtualizedFilesystem[F[_]: Monad](
  roots: Map[String, (Path, Filesystem[F])],
  runTmpFs: Filesystem[F]
)(
  implicit val fsEq: Equal[Filesystem[F]]
) extends Filesystem[F] {
  private def retranslate(path: Path): (Path, Filesystem[F], String) = {
    path.segments match {
      case Vector() => (Path.root, this, "")
      case head +: rest =>
        val (root, fs) = roots(head)
        (root / rest, fs, head)
    }
  }

  private def unaryOp[A](path: Path)(op: (Filesystem[F], Path) => A): A = {
    val (p, fs, _) = retranslate(path)
    op(fs, p)
  }

  override def childrenOf(path: Path) = {
    val (p, fs, head) = retranslate(path)
    fs.childrenOf(p)
      .map { paths => paths.map(Path(head) / _.relStringTo(p)) }
  }

  override def getBytes(path: Path) =
    unaryOp(path) { _ getBytes _ }

  override def setBytes(path: Path, cnt: ImmutableArray[Byte]) =
    unaryOp(path) { _ setBytes (_, cnt) }

  override def mkDir(path: Path) =
    unaryOp(path) { _ mkDir _ }

  private def slowCopy(from: Path, to: Path): F[Unit] = {
    def fileCopy = getBytes(from) >>= { setBytes(to, _) }
    def folderCopy = for {
      children <- childrenOf(from)
      _ <- children.traverse_ { path =>
        slowCopy(path, to / path.relTo(from))
      }
    } yield ()

    isRegularFile(from).ifM(fileCopy, folderCopy)
  }

  override def copy(source: Path, dest: Path) = {
    val (p1, fs1, _) = retranslate(source)
    val (p2, fs2, _) = retranslate(dest)
    if (fs1 === fs2) fs1.copy(p1, p2)
    else slowCopy(source, dest)
  }

  override def move(source: Path, dest: Path) = {
    val (p1, fs1, _) = retranslate(source)
    val (p2, fs2, _) = retranslate(dest)
    if (fs1 === fs2) fs1.move(p1, p2)
    else slowCopy(source, dest) >> rmTree(source)
  }

  override def rmTree(path: Path) =
    unaryOp(path) { _ rmTree _ }

  override def stat(path: Path) =
    unaryOp(path) { _ stat _ }

  override def runTmp[A](f: Path => F[A]) =
    runTmpFs.runTmp(f)

  override protected def hashFile(path: Path) =
    unaryOp(path) { _ hashAt _ }
}
