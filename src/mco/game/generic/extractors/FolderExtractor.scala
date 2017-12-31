package mco.game.generic.extractors

import cats.Functor
import mco.core.paths.{Path, Pointed, RelPath}
import mco.io.Filesystem
import cats.syntax.functor._

class FolderExtractor[F[_]: Filesystem: Functor](dir: Path) extends Extractor[F] {
  private def descendantsOf(path: Path): fs2.Stream[F, Path] =
    fs2.Stream.eval(Filesystem.isDirectory(path)).flatMap {
      case true =>
        fs2.Stream.eval(Filesystem.childrenOf(path))
          .flatMap(fs2.Stream.emits(_))
          .flatMap(descendantsOf)
      case false =>
        fs2.Stream.emit(path)
    }


  override def entries: fs2.Stream[F, RelPath] =
    descendantsOf(dir).map(_ relTo dir)

  override def provide(children: Set[RelPath]): fs2.Stream[F, Pointed[Path]] =
    entries.filter(children).map(rel => (rel, dir / rel))
}

object FolderExtractor {
  def apply[F[_]: Filesystem: Functor]: Extractor.Provider[F] = path =>
    Filesystem.isDirectory(path).map {
      case true  => Some(new FolderExtractor(path))
      case false => None
    }
}