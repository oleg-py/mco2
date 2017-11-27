package mco.io.generic

import net.sf.sevenzipjbinding.{IInStream, IOutStream}
import scala.collection.breakOut
import scala.collection.mutable
import scala.collection.immutable.SortedMap
import scala.util.Try
import scalaz._
import syntax.functor._

import better.files.File.RandomAccessMode
import better.files._
import mco.core.Capture
import mco.data.paths._
import mco.util.syntax.any._
import net.sf.sevenzipjbinding._
import net.sf.sevenzipjbinding.impl.{RandomAccessFileInStream, RandomAccessFileOutStream}

import java.io.Closeable


class ArchivingImpl[F[_]: Capture] private (
  getInStream: Path => IInStream,
  getOutStream: Path => IOutStream with Closeable
) extends Archiving[F] {
  override def entries(archive: Path): F[Vector[RelPath]] = Capture {
    val inArchive = SevenZip.openInArchive(null, getInStream(archive))
    val result = inArchive.getSimpleInterface.getArchiveItems
      .collect {
        case entry if !entry.isFolder => rel"${entry.getPath}"
      }
      .toVector

    inArchive.close()

    result
  }

  override def extract(archive: Path, targets: Map[RelPath, Path]): F[Unit] = Capture {
    val inArchive = SevenZip.openInArchive(null, getInStream(archive))

    val entries: SortedMap[Int, Path] = inArchive.getSimpleInterface
      .getArchiveItems
      .zipWithIndex
      .collect {
        case (entry, i) if !entry.isFolder && targets.contains(rel"${entry.getPath}") =>
          i -> targets(rel"${entry.getPath}")
      }(breakOut)

    val files = mutable.HashSet.empty[Closeable]

    inArchive.extract(entries.keysIterator.toArray, false, new IArchiveExtractCallback {
      override def prepareOperation(extractAskMode: ExtractAskMode): Unit = ()
      override def setOperationResult(extractOperationResult: ExtractOperationResult): Unit = ()
      override def getStream(index: Int, extractAskMode: ExtractAskMode): ISequentialOutStream = {
        entries.get(index)
          .filter(_ => extractAskMode == ExtractAskMode.EXTRACT)
          .map(getOutStream)
          .map(_.tap(files += _))
          .orNull
      }
      override def setTotal(total: Long): Unit = ()
      override def setCompleted(complete: Long): Unit = ()
    })

    files.foreach (f => Try { f.close() })
    inArchive.close()
  }
}

object ArchivingImpl {
  private lazy val initLibraryOnce: Unit = SevenZip.initSevenZipFromPlatformJAR()

  def apply[F[_]: Capture: Functor] (
    getInStream: Path => IInStream,
    getOutStream: Path => IOutStream with Closeable
  ): F[Archiving[F]] = Capture(initLibraryOnce)
    .as(new ArchivingImpl[F](getInStream, getOutStream))
}
