package mco.game.generic.extractors

import cats.Functor
import cats.data.OptionT
import cats.syntax.all._
import mco.core.paths.{Path, RelPath, Segment}
import mco.io.Filesystem


class CachedExtractor[F[_]: Filesystem: Functor] private (
  cacheDir: Path,
  name: Segment,
  base: Extractor[F]
) extends Extractor[F] {
  implicit private val streamSync = fs2.Stream.syncInstance[F]

  /*_*/
  private def ensureCache: fs2.Stream[F, FolderExtractor[F]] = {
    val target = cacheDir / name
    def extractFiles = base.provideAll.evalMap { case (rel, path) =>
      Filesystem.copy(path, target / rel)
    }

    streamSync.ifM(fs2.Stream.eval(Filesystem.exists(target)))(
      fs2.Stream.empty,
      extractFiles
    ).drain ++ fs2.Stream.emit(new FolderExtractor(target))
  }
  /*_*/

  override def entries: fs2.Stream[F, RelPath] =
    ensureCache.flatMap(_.entries)

  override def provide(children: Set[RelPath]): fs2.Stream[F, (RelPath, Path)] =
    ensureCache.flatMap(_.provide(children))
}

object CachedExtractor {
  def apply[F[_]: Functor: Filesystem](
    cacheDir: Path,
    base: Extractor.Provider[F]
  ): Extractor.Provider[F] = path => OptionT(base(path))
    .map { new CachedExtractor(cacheDir, path.name, _) }
    .value
    .widen
}