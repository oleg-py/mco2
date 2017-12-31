package mco.game.generic.extractors

import cats.Functor
import mco.core.paths.{Path, Pointed, RelPath}
import mco.io.Filesystem
import cats.syntax.functor._

class FolderExtractor[F[_]: Filesystem](dir: Path) extends Extractor[F] {
  override def entries: fs2.Stream[F, RelPath] =
    fs2.Stream.eval(Filesystem.childrenOf(dir))
      .flatMap(fs2.Stream.emits(_))
      .map(_ relTo dir)

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