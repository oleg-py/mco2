package mco.stubs

import scalaz._

import mco.core.Capture
import mco.core.paths.Path
import mco.core.vars.Var
import mco.io.{Archiving, Filesystem}
import mco.io.impls.InMemoryArchiving
import mco.stubs.cells._
import mco.util.syntax.fp._
import mco.util.syntax.??

import java.net.URL
import java.nio.file.attribute.BasicFileAttributes
import java.util.Base64


class VarFilesystem[F[_]: Monad: Capture] (rootVar: Var[F, Cell])
  extends Filesystem[F]
{

  override def archiving: Archiving[F] = new InMemoryArchiving[F]()(this, ??, ??)

  private def complainAbout(path: Path) = sys.error(s"Assertion failure at $path")

  def deepGet(path: Path): F[Option[Cell]] =
    rootVar().map(_.lookup(path.relTo(Path.root)))

  def deepSet(path: Path)(obj: Option[Cell]): F[Unit] =
    rootVar ~= { _
      .update(path.relTo(Path.root), obj)
      .getOrElse(complainAbout(path))
    }

  private def check(pf: PartialFunction[Option[Cell], Unit])= (path: Path) =>
    deepGet(path) map { x =>
      if (!pf.isDefinedAt(x)) complainAbout(path)
    }

  private val notFolder = check { case None | Some(File(_)) => }
  private val notFile   = check { case None | Some(Dir(_)) => }
  private val mustExist = check { case Some(_) => }

  override def childrenOf(path: Path): F[Stream[Path]] =
    deepGet(path) map {
      case Some(Dir(cc)) => cc.keys.map(path / _).toStream
      case _ => complainAbout(path)
    }

  override def getBytes(path: Path): F[Array[Byte]] =
    deepGet(path) map {
      case Some(File(arr)) => arr
      case _ => complainAbout(path)
    }

  override def setBytes(path: Path, cnt: Array[Byte]): F[Unit] =
    notFolder(path) >>
      deepSet(path)(File(cnt).some)

  override def mkDir(path: Path): F[Unit] =
    notFile(path) >>
      deepGet(path).map(_ orElse Dir(Map()).some) >>=
      deepSet(path)

  override def copy(source: Path, dest: Path): F[Unit] =
    mustExist(source) >>
      deepGet(source) >>=
      deepSet(dest)

  override def move(source: Path, dest: Path): F[Unit] =
    copy(source, dest) >>
      rmTree(source)

  override def rmTree(path: Path): F[Unit] =
    mustExist(path) >>
      deepSet(path)(None)

  override def stat(path: Path): F[Option[BasicFileAttributes]] =
    deepGet(path) map { _.map(StubAttributes(path, _)) }

  override protected[mco] def hashFile(p: Path): F[(Long, Long)] = for {
    file <- deepGet(p)
    Some(File(bs)) = file
  } yield (bs.length.toLong, bs.map(_.toLong).sum)

  val stdTmpDir = Path("/tmp/$buffer")

  override def runTmp[A](f: Path => F[A]): F[A] =
    for {
      _ <- ensureDir(stdTmpDir)
      a <- f(stdTmpDir)
      _ <- rmTree(stdTmpDir)
    } yield a

  override def fileToUrl(p: Path) = for {
    file <- deepGet(p)
    Some(File(bs)) = file
  } yield new URL("data:;base64," + new String(Base64.getEncoder.encode(bs)))
}
