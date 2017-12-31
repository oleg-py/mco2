package mco.game.generic.extractors

import cats.{Functor, Monad}
import cats.syntax.functor._
import cats.syntax.flatMap._
import mco.core.paths.{Path, Pointed, RelPath}
import mco.io.{Archive, Archiving, Filesystem}


class ArchiveExtractor[F[_]: Filesystem: Functor](archive: Archive[F]) extends Extractor[F] {
  override def entries: fs2.Stream[F, RelPath] = archive.entries
  override def provide(children: Set[RelPath]): fs2.Stream[F, Pointed[Path]] =
    Filesystem.mkTemp
      .evalMap { tmp =>
        val paths = children.toVector.map(rel => (rel, tmp / rel))
        archive.extract(paths.toMap).as(paths)
      }
      .flatMap(fs2.Stream.emits(_))
}

object ArchiveExtractor {
  private val supported = Set(".7z", ".zip", ".rar")

  def apply[F[_]: Monad: Filesystem: Archiving]: Extractor.Provider[F] = path =>
    for {
      isFile    <- Filesystem.isRegularFile(path)
      extension =  path.extension
      ok        =  isFile && supported(extension)
      arch      <- Archiving.asArchive(path)
    } yield if (ok) Some(new ArchiveExtractor[F](arch))
            else    None
}