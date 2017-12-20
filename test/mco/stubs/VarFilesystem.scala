package mco.stubs

import cats._
import cats.syntax.all._
import mco.core.paths.Path
import mco.core.vars.Var
import mco.io.Filesystem
import mco.stubs.cells._
import net.sf.sevenzipjbinding.util.ByteArrayStream
import net.sf.sevenzipjbinding.{IInStream, IOutStream}

import java.net.URL
import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes
import java.util.Base64


class VarFilesystem[F[_]: Monad] (rootVar: Var[F, Cell])
  extends Filesystem[F]
{

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

  private def getBytes(path: Path): F[Array[Byte]] =
    deepGet(path) map {
      case Some(File(arr)) => arr
      case _ => complainAbout(path)
    }

  private def setBytes(path: Path, cnt: Array[Byte]): F[Unit] =
    notFolder(path) *>
      deepSet(path)(File(cnt).some)

  override def mkDir(path: Path): F[Unit] =
    notFile(path) *>
      deepGet(path).map(_ orElse Dir(Map()).some) >>=
      deepSet(path)

  override def copy(source: Path, dest: Path): F[Unit] =
    mustExist(source) *>
      deepGet(source) >>=
      deepSet(dest)

  override def move(source: Path, dest: Path): F[Unit] =
    copy(source, dest) *>
      rmTree(source)

  override def rmTree(path: Path): F[Unit] =
    mustExist(path) *>
      deepSet(path)(None)

  override def stat(path: Path): F[Option[BasicFileAttributes]] =
    deepGet(path) map { _.map(StubAttributes(path, _)) }

  val stdTmpDir = Path("/tmp/$buffer")

  override def readFile(path: Path): fs2.Stream[F, ByteBuffer] =
    fs2.Stream.eval(getBytes(path).map(ByteBuffer.wrap(_)))

  override def writeFile(path: Path, bb: ByteBuffer): F[Unit] = {
    val arr = Array.ofDim[Byte](bb.remaining())
    bb.get(arr)
    setBytes(path, arr)
  }

  override def mkTemp: fs2.Stream[F, Path] =
    fs2.Stream.bracket(ensureDir(stdTmpDir))(
      _ => fs2.Stream(stdTmpDir),
      _ => rmTree(stdTmpDir)
    )

  override def getSfStream(path: Path): fs2.Stream[F, IInStream with IOutStream] = {
    val acquire = notFolder(path).followedBy(deepGet(path)).map {
      case Some(File(bytes)) => new ByteArrayStream(bytes, false)
      case _ => new ByteArrayStream(Int.MaxValue)
    }

    fs2.Stream.bracket(acquire)(
      r => fs2.Stream(r),
      bas => deepSet(path)(File(bas.getBytes).some)
    ).map(x => x: IInStream with IOutStream)
  }

  override def fileToUrl(p: Path) = for {
    file <- deepGet(p)
    Some(File(bs)) = file
  } yield new URL("data:;base64," + new String(Base64.getEncoder.encode(bs)))
}
