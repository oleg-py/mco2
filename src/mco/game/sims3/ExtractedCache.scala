package mco.game.sims3

import mco.core.paths.{Path, Pointed, RelPath}


trait ExtractedCache[F[_]] {
  def contains(xs: RelPath): F[Boolean]
  def getOrEmpty(xs: RelPath): fs2.Stream[F, Pointed[Path]]
  def put(xs: RelPath, fa: fs2.Stream[F, Pointed[Path]]): F[Unit]
}
