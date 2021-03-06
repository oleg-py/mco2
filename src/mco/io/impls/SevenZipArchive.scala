package mco.io.impls

import mco.io.{Archive, Filesystem}
import cats.Monad
import cats.effect.Sync
import cats.implicits._
import mco.syntax._
import mco.core.paths._
import net.sf.sevenzipjbinding._

private[impls] class SevenZipArchive[F[_]: Filesystem: Sync](
  file: Path
) extends Archive[F] {
  implicit def streamMonad: Monad[fs2.Stream[F, ?]] = fs2.Stream.syncInstance[F]

  def entries: fs2.Stream[F, RelPath] =
    getInArchive.flatMap(emitEntries)

  def extract(rawTargets: Map[RelPath, Path]): F[Unit] = {
    val streamed = for {
      archive  <- getInArchive
      targets  <- filterTargets(archive, rawTargets)
      mapped   <- mapForOutput(targets)
      idxMap   <- fs2.Stream.eval(getIndexMap(archive, targets.keys))
      outMap   =  idxMap.mapValues(mapped)
      _        <- fs2.Stream.eval(runExtraction(archive, outMap))
    } yield ()

    streamed.runSync
  }

  private def getInArchive: fs2.Stream[F, IInArchive] =
    Filesystem.getSfStream(file).flatMap { input =>
      val arch = capture(SevenZip.openInArchive(null, input))
      fs2.Stream.bracket(arch)(
        x => fs2.Stream(x),
        archive => capture(archive.close())
      )
    }

  private def filterTargets(archive: IInArchive, rawTargets: Map[RelPath, Path]) =
    emitEntries(archive).foldMap(Set(_))
      .map(paths => rawTargets.filterKeys(paths))

  private def emitEntries(arch: IInArchive) = fs2.Stream.suspend {
    val seq = arch.getSimpleInterface.getArchiveItems
      .collect {
        case entry if !entry.isFolder => rel"${entry.getPath}"
      }
    fs2.Stream.emits(seq).covary[F]
  }

  private def mapForOutput(targets: Map[RelPath, Path]) = {
    val (keys, vals) = targets.unzip
    vals.toList
      .traverse(Filesystem.getSfStream(_))
      .map { newVals => keys.zip(newVals).toMap }
  }

  private def getIndexMap(
    archive: IInArchive,
    keys: Iterable[RelPath]
  ): F[Map[Int, RelPath]] =
    capture {
      val keySet = keys.toSet
      archive.getSimpleInterface
        .getArchiveItems
        .filter(entry => keySet(RelPath(entry.getPath)))
        .map(entry => (entry.getItemIndex, RelPath(entry.getPath)))
        .toMap
    }

  private def runExtraction(archive: IInArchive, outMap: Map[Int, IOutStream]): F[Unit] =
    capture {
      archive.extract(null, false, new IArchiveExtractCallback {
        override def getStream(index: Int, extractAskMode: ExtractAskMode): IOutStream =
          outMap.get(index).orNull

        override def prepareOperation(extractAskMode: ExtractAskMode): Unit = ()

        override def setOperationResult(
          extractOperationResult: ExtractOperationResult): Unit = ()

        override def setTotal(total: Long): Unit = ()
        override def setCompleted(complete: Long): Unit = ()
      })
    }
}
