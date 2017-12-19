package mco.game.generic

import cats.FlatMap
import mco.core.paths.{Path, RelPath}
import mco.core.state.{ModState, RepoState}
import mco.core.vars.Var
import mco.io.Filesystem
import mco.syntax._

class ContentTraversalFS[F[_]: Filesystem: FlatMap](
  val resolve: NameResolver,
  repoVar: Var[F, RepoState],
  targetMod: RelPath
) extends ContentTraversal[F] {
  override def copy(from: Path, to: Path): F[Unit] =
    Filesystem.copy(from, to)

  override def remove(at: Path): F[Unit] =
    Filesystem.rmTree(at)

  override def track(rel: RelPath, value: Option[Path]): F[Unit] =
    repoVar ~= RepoState.pathL(targetMod)
      .composeLens(ModState.contents)
      .modify { _.alter(rel, {
        case Some(cs) => Some(cs.copy(target = value))
        case None => None
      }) }
}
