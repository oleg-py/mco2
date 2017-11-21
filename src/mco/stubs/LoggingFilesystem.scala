package mco.stubs

import scalaz.{ImmutableArray, Monad}

import mco.data.Path
import mco.io.generic.Filesystem

//noinspection TypeAnnotation
class LoggingFilesystem[F[_]: Monad](inner: Filesystem[F]) extends Filesystem[F] {
  override def childrenOf(path: Path) = {
    pprint.log(path.asString)
    inner.childrenOf(path)
  }

  override def getBytes(path: Path) = {
    pprint.log(path.asString)
    inner.getBytes(path)
  }

  override def setBytes(path: Path, cnt: ImmutableArray[Byte]) = {
    pprint.log(path.asString)
    pprint.log(s"bytes len: ${cnt.length}")
    inner.setBytes(path, cnt)
  }

  override def mkDir(path: Path) = {
    pprint.log(path.asString)
    inner.mkDir(path)
  }

  override def copy(source: Path, dest: Path) = {
    pprint.log(source.asString, "from")
    pprint.log(dest.asString, "to")
    inner.copy(source, dest)
  }

  override def move(source: Path, dest: Path) = {
    pprint.log(source.asString, "from")
    pprint.log(dest.asString, "to")
    inner.move(source, dest)
  }

  override def rmTree(path: Path) = {
    pprint.log(path.asString)
    inner.rmTree(path)
  }

  override def stat(path: Path) = {
    pprint.log(path.asString)
    inner.stat(path)
  }

  override def runTmp[A](f: Path => F[A]) = {
    pprint.log("Doing runtmp...")
    inner.runTmp(f)
  }

  override protected[mco] def hashFile(p: Path) = {
    pprint.log(p.asString)
    inner.hashFile(p)
  }
}
