package mco.game.sims3

import cats.Monad
import mco.core.Mod
import mco.core.paths.{Path, Pointed, RelPath}
import cats.implicits._

class FlatSims3Pack[F[_]: Monad](
  cache: ExtractedCache[F]
)(
  underlying: Mod[F]
) extends Mod[F] {
  override val backingFile: Path = underlying.backingFile
  override def list: F[Vector[RelPath]] =
    for {
      paths <- underlying.list
      (packs, rest) = paths.partition(_.extension.toLowerCase == ".sims3pack")
      flattened <- packContents(packs)
    } yield flattened ++ rest

  override def provide(content: Vector[RelPath]): fs2.Stream[F, Pointed[Path]] =
    underlying.provide(content).flatMap {
      case (rel, path) if rel.extension.toLowerCase == ".sims3pack" =>
        ???
      case tuple => fs2.Stream.emit(tuple)
    }

  private def packContents(xs: Vector[RelPath]): F[Vector[RelPath]] = {
    val separateL = xs.foldMapM(path => cache.contains(path).ifM(
      (Vector.empty[RelPath], Vector(path)).pure[F],
      (Vector(path), Vector.empty[RelPath]).pure[F]
    ))

    ???
  }
}
