package mco.io.impls

import scalaz.Monad
import scalaz.std.list._

import mco.core.Capture
import mco.core.paths._
import mco.io.{Archiving, Filesystem}
import mco.util.syntax.fp._
import net.sf.sevenzipjbinding.util.ByteArrayStream


class InMemoryArchiving[F[_]: Filesystem: Monad: Capture] extends Archiving[F] {
  private val MaxStreamSize = 32 * 1024 * 1024 // bytes

  override def entries(archive: Path): F[Vector[RelPath]] =
    for {
      data   <- Filesystem.getBytes(archive)
      in     =  new ByteArrayStream(data, false)
      result <- new SevenZipArchiving[F](_ => in, Map()).entries(archive)
    } yield result

  private def writeOutputs(outStreams: Map[Path, ByteArrayStream]) =
    outStreams.toList.traverse_ { case (path, stream) =>
      Filesystem.setBytes(path, stream.getBytes)
    }

  override def extract(archive: Path, targets: Map[RelPath, Path]): F[Unit] =
    for {
      data <- Filesystem.getBytes(archive)
      in   =  new ByteArrayStream(data, false)
      outs =  targets.map { case (_, to) => to -> new ByteArrayStream(MaxStreamSize) }
      _    <- new SevenZipArchiving(_ => in, outs).extract(archive, targets)
      _    <- writeOutputs(outs)
    } yield ()
}
