package mco.stubs

import scalaz._
import std.anyVal._
import std.option._
import syntax.id._
import syntax.std.map._

import mco.io.generic.Filesystem
import mco.stubs.Cell._
import mco.util.syntax.fp._

import java.nio.file.attribute.BasicFileAttributes
import mco.core.vars.Var
import mco.data.paths.{Path, Segment}

import java.net.URL
import java.util.Base64


class VarFilesystem[F[_]: Monad] (rootVar: Var[F, Dir])
  extends Filesystem[F]
{
  import Cell._

  private def complainAbout(path: Path) = sys.error(s"Assertion failure at $path")

  def deepGet(path: Path): F[Option[Cell]] =
    for (root <- rootVar()) yield
      path.segments.foldLeft(some[Cell](root)) {
        case (Some(Dir(cc)), key) => cc.get(key.toString)
        case _ => None
      }

  def deepSet(path: Path)(obj: Option[Cell]): F[Unit] = {
    def recurse(segments: List[Segment])(parent: Dir): Dir = {
      val cs = parent.contents
      segments match {
        case Nil       => complainAbout(path)
        case Segment(s) :: Nil  => obj.cata(cs.updated(s, _), cs - s) |> Dir
        case Segment(s) :: more => cs.alter(s) {
          case Some(_: File) => complainAbout(path)
          case Some(d: Dir)  => recurse(more)(d).some
          case None          => recurse(more)(Dir()).some
        } |> Dir
      }
    }

    rootVar ~= recurse(path.segments.toList)
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
      case Some(Dir(cc)) => cc.keys.map(Segment(_)).map(path / _).toStream
      case _ => complainAbout(path)
    }

  override def getBytes(path: Path): F[ImmutableArray[Byte]] =
    deepGet(path) map {
      case Some(File(arr)) => ImmutableArray.fromArray(arr)
      case _ => complainAbout(path)
    }

  override def setBytes(path: Path, cnt: ImmutableArray[Byte]): F[Unit] =
    notFolder(path) >>
      deepSet(path)(File(cnt.toArray).some)

  override def mkDir(path: Path): F[Unit] =
    notFile(path) >>
      deepGet(path).map(_ orElse Dir().some) >>=
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
