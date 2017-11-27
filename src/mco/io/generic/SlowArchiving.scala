package mco.io.generic

import scalaz.{ImmutableArray, Monad}
import scalaz.std.list._

import mco.core.Capture
import mco.data.paths.{Path, RelPath}
import mco.util.syntax.fp._
import net.sf.sevenzipjbinding.util.ByteArrayStream
import net.sf.sevenzipjbinding.{IInStream, IOutStream}

import java.io.Closeable


class SlowArchiving[F[_]: Filesystem: Monad: Capture] extends Archiving[F] {
  private class Delegate(inStream: IInStream, outStreams: Map[Path, ByteArrayStream])
    extends ArchivingImpl[F]
  {
    override protected def getInStream(path: Path): IInStream = inStream
    override protected def getOutStream(path: Path): IOutStream with Closeable =
      outStreams(path)
  }

  private val MaxStreamSize = 32 * 1024 * 1024 // bytes

  override def entries(archive: Path): F[Vector[RelPath]] =
    for {
      data    <- Filesystem.getBytes(archive)
      in       = new ByteArrayStream(data.toArray, false)
      delegate = new Delegate(in, Map())
      result  <- delegate.entries(archive)
    } yield result

  private def writeOutputs(outStreams: Map[Path, ByteArrayStream]) =
    outStreams.toList.traverse_ { case (path, stream) =>
      Filesystem.setBytes(path, ImmutableArray.fromArray(stream.getBytes))
    }

  override def extract(archive: Path, targets: Map[RelPath, Path]): F[Unit] =
    for {
      data    <- Filesystem.getBytes(archive)
      in       = new ByteArrayStream(data.toArray, false)
      outs     = targets.map { case (_, to) => to -> new ByteArrayStream(MaxStreamSize) }
      delegate = new Delegate(in, outs)
      _       <- delegate.extract(archive, targets)
      _       <- writeOutputs(outs)
    } yield ()
}
