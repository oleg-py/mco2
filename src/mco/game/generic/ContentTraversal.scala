package mco.game.generic

import mco.core.paths.{Path, RelPath}


/**
 * Algebra of small operations required for mod copying
 * that supports full conflict resolution
 */
trait ContentTraversal[F[_]] {
  val resolve: NameResolver
  def copy(from: Path, to: Path): F[Unit]
  def remove(at: Path): F[Unit]
  def track(rel: RelPath, value: Option[Path]): F[Unit]
}
