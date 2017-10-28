package mco.stubs

import scalaz._
import syntax.std.option._

import mco.util.syntax.fp._
import std.anyVal._
import std.stream._
import std.tuple._

import Cell._
import mco.data.Path
import mco.io.generic.Filesystem
import mco.io.state.Hashing

import java.nio.file.attribute.BasicFileAttributes


object StateInterpreter
  extends Filesystem[FState]
     with Hashing[FState]
{
  override def childrenOf(path: Path): FState[Stream[Path]] =
    deepGet(path) map {
      case Some(Dir(cc)) => cc.keys.map(path / _).toStream
      case _ => complainAbout(path)
    }

  override def getBytes(path: Path): FState[ImmutableArray[Byte]] =
    deepGet(path) map {
      case Some(File(arr)) => arr
      case _ => complainAbout(path)
    }

  override def setBytes(path: Path, cnt: ImmutableArray[Byte]): FState[Unit] =
    notFolder(path) >>
      deepSet(path)(File(cnt).some)

  override def mkDir(path: Path): FState[Unit] =
    notFile(path) >>
      deepGet(path).map(_ orElse Dir().some) >>=
      deepSet(path)

  override def copy(source: Path, dest: Path): FState[Unit] =
    mustExist(source) >>
      deepGet(source) >>=
      deepSet(dest)

  override def move(source: Path, dest: Path): FState[Unit] =
    copy(source, dest) >>
      rmTree(source)

  override def rmTree(path: Path): FState[Unit] =
    mustExist(path) >>
      deepSet(path)(None)

  override def stat(path: Path): FState[Option[BasicFileAttributes]] =
    deepGet(path) map { _.map(StubAttributes(path, _)) }

  override def hashFile(p: Path): FState[(Long, Long)] = {
    def hash(cell: Cell): (Long, Long) = cell match {
      case Dir(cs)  => cs.values.toStream.foldMap(hash)
      case File(bs) => (bs.length.toLong, bs.foldMap(_.toLong))
    }
    for (opt <- deepGet(p)) yield opt.map(hash).orZero
  }
}
