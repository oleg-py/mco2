package mco.game.sims3.extractors

import cats.Functor
import cats.syntax.functor._
import mco.core.paths._
import mco.game.generic.extractors.Extractor
import mco.game.sims3.{S3CE, Sims3PackContent}
import mco.io.Filesystem


class Sims3PackExtractor[F[_]](c: Sims3PackContent[F]) extends Extractor[F] {
  override def entries: fs2.Stream[F, RelPath] =
    c.unpack.map(path => rel"${path.name}")

  override def provide(children: Set[RelPath]): fs2.Stream[F, Pointed[Path]] =
    c.unpack
      .map(path => rel"${path.name}" -> path)
      .filter { case (rel, _) => children(rel) }
}

object Sims3PackExtractor {
  def apply[F[_]: Filesystem: Functor: S3CE]: Extractor.Provider[F] = path => {
    val isPack = path.extension == ".sims3pack"
    Filesystem.isRegularFile(path) map {
      case true if isPack =>
        val content = new Sims3PackContent[F](implicitly[S3CE[F]], path)
        Some(new Sims3PackExtractor(content))
      case _ => None
    }
  }
}