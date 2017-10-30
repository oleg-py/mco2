package mco.io.generic

import scalaz._
import std.vector._

import mco.core.state.RepoState
import mco.core._
import mco.data.{Key, Path}
import mco.util.syntax.fp._
import TempFolder._, Filesystem._, Content._


class LocalMods[F[_]: Monad: Filesystem: TempFolder](
  mods: Map[Key, Mod[F]],
  resolverOf: Content.Plain => NameResolver[F]
) extends Mods[F] {
  override def state: F[RepoState] = ???

  override def update(key: Key, diff: Deltas.OfMod) = ???

  private def install(key: Key) = runTmp[F, Unit] { tmpDir =>
    for {
      rState <- state
      modIdx    =  rState.orderedMods.indexWhere(_.key == key)
      modState  =  rState.orderedMods(modIdx).strengthR(mods(key))
      resolved  <- resolverOf(Component).resolveAll(Component, modState).apply(tmpDir)
      hasConflicts = rState.hasConflicts(_: Path, modIdx)
      _ <- resolved.traverse_ {
        case (from, Some(to)) if !hasConflicts(to) => copy(from, to)
        case _ => ().point[F]
      }
    } yield ()
  }

  //noinspection ConvertibleToMethodValue
  private def uninstall(key: Key) = for {
    repoState <- state
    modIdx = repoState.orderedMods.indexWhere(_.key == key)
    paths = repoState.orderedMods(modIdx).get.contents.toVector.flatMap(_.target)
    _ <- paths.traverse_(rmTree(_))
  } yield ()
}
