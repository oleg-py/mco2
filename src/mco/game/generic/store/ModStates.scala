package mco.game.generic.store

import cats.Traverse.nonInheritedOps.toTraverseOps
import cats.effect.Sync
import cats.implicits._
import mco.core._
import mco.core.paths._
import mco.core.state._
import mco.game.generic.NameResolver
import mco.io.FileStamping


class ModStates[F[_]: Sync: FileStamping](
  root: Path,
  nameResolver: NameResolver,
  kindOf: RelPath => ContentKind,
  known: Vector[Pointed[ModState]]
) {
  private def childState(parent: Path)(pp: Pointed[Path]) = {
    val resolved = nameResolver(parent.relTo(root), pp._1)
    val update = FileStamping.overwrite((parent, pp._1), resolved)

    update.map { _ =>
      Vector(pp._1 -> ContentState(Status.Installed, kindOf(pp._1)))
    }
  }

  private def recompute(mod: Mod[F], index: Int): F[ModState] = {
    val result = mod.provideAll
      .evalMap(childState(mod.backingFile))
      .runFoldMonoidSync

    result.map { cs => ModState(Status.Unused, cs.toMap) }
  }

  def computeAll(mods: Vector[Mod[F]]): F[RepoState] =
    mods.traverse(computeState).map { computed =>
      val (states, labels) = (mods zip computed)
        .map { case (mod, state) =>
          val path = mod.backingFile.relTo(root)
          ((path, state), (path, mod.label))
        }
        .unzip
      RepoState(states, labels.toMap)
    }

  def computeState(mod: Mod[F]): F[ModState] = {
    val file = mod.backingFile
    val needsRecompute = FileStamping.likelySame(
      (file, rel""), file, file
    ).map(x => !x)
    def update = (i: Int) =>
      recompute(mod, i) <* FileStamping.overwrite((file, rel""), file)

    val index =  known.indexWhere(_._1 == file.relTo(root))
    val isNew =  index == -1
    for {
      stale    <- needsRecompute
      modState <- if (isNew || stale) update(index)
                  else known(index)._2.pure[F]
    } yield modState
  }
}
