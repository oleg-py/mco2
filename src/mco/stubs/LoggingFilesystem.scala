package mco.stubs

import scalaz.Monad

import mco.core.paths.Path
import mco.io.{Archiving, Filesystem}

//noinspection TypeAnnotation
class LoggingFilesystem[F[_]: Monad](inner: Filesystem[F]) extends Filesystem[F] {
  override def archiving: Archiving[F] = inner.archiving

  override def childrenOf(path: Path) = {
    pprint.log(path.toString)
    inner.childrenOf(path)
  }

  override def getBytes(path: Path) = {
    pprint.log(path.toString)
    inner.getBytes(path)
  }

  override def setBytes(path: Path, cnt: Array[Byte]) = {
    pprint.log(path.toString)
    pprint.log(s"bytes len: ${cnt.length}")
    inner.setBytes(path, cnt)
  }

  override def mkDir(path: Path) = {
    pprint.log(path.toString)
    inner.mkDir(path)
  }

  override def copy(source: Path, dest: Path) = {
    pprint.log(source.toString, "from")
    pprint.log(dest.toString, "to")
    inner.copy(source, dest)
  }

  override def move(source: Path, dest: Path) = {
    pprint.log(source.toString, "from")
    pprint.log(dest.toString, "to")
    inner.move(source, dest)
  }

  override def rmTree(path: Path) = {
    pprint.log(path.toString)
    inner.rmTree(path)
  }

  override def stat(path: Path) = {
    pprint.log(path.toString)
    inner.stat(path)
  }

  override def runTmp[A](f: Path => F[A]) = {
    pprint.log("Doing runtmp...")
    inner.runTmp(f)
  }

  override protected[mco] def hashFile(p: Path) = {
    pprint.log(p.toString)
    inner.hashFile(p)
  }

  override def fileToUrl(p: Path) = {
    pprint.log(p.toString)
    inner.fileToUrl(p)
  }
}
