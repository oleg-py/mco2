package mco.stubs

import cats._
import mco.core.paths.Path
import mco.io.Filesystem
import net.sf.sevenzipjbinding.{IInStream, IOutStream}

import java.nio.ByteBuffer

//noinspection TypeAnnotation
class LoggingFilesystem[F[_]: Monad](inner: Filesystem[F]) extends Filesystem[F] {

  override def readFile(path: Path): fs2.Stream[F, ByteBuffer] = {
    pprint.log(path)
    inner.readFile(path)
  }

  override def writeFile(path: Path, bb: ByteBuffer): F[Unit] = {
    pprint.log(path)
    pprint.log(bb.remaining())
    inner.writeFile(path, bb)
  }

  override def mkTemp: fs2.Stream[F, Path] = {
    pprint.log("mkTemp")
    inner.mkTemp
  }

  override def getSfStream(path: Path): fs2.Stream[F, IInStream with IOutStream] = {
    pprint.log(path)
    inner.getSfStream(path)
  }

  override def childrenOf(path: Path) = {
    pprint.log(path.toString)
    inner.childrenOf(path)
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

  override def fileToUrl(p: Path) = {
    pprint.log(p.toString)
    inner.fileToUrl(p)
  }
}
