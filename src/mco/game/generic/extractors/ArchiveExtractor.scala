package mco.game.generic.extractors

import cats.Functor
import cats.effect.Sync
import cats.syntax.functor._
import mco.core.paths.{Path, Pointed, RelPath}
import mco.io.{Archive, Filesystem}


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

  def apply[F[_]: Sync: Filesystem]: Extractor.Provider[F] = path =>
    for {
      isFile    <- Filesystem.isRegularFile(path)
      extension =  path.extension
      ok        =  isFile && supported(extension)
    } yield if (ok) Some(new ArchiveExtractor[F](new Archive[F](path)))
            else    None
}