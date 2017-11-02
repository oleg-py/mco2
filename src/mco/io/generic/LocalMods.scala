package mco.io.generic

import scalaz._
import std.vector._
import mco.core.state._
import mco.core._
import mco.data._
import mco.util.syntax.fp._
import TempFolder._
import Filesystem._
import monocle.function.Index.index
import monocle.macros.syntax.lens._


class LocalMods[F[_]: Monad: Filesystem: TempFolder](
  mods: Map[Key, Mod[F]],
  resolverOf: Content.Plain => NameResolver[F]
) extends Mods[F] {
  override def state: F[RepoState] = ???

  override def update(key: Key, diff: Deltas.OfMod) = ???

  private def modLens(i: Int) =
    RepoState.orderedMods composeOptional
    index(i) composeLens
    Keyed.lens

  private def setPaths(updates: Vector[(Key, Path)]) =
    ModState.contents modify { modMap =>
      updates.foldLeft(modMap) { case (map, (key, path)) =>
        map.adjust(key, _
          .lens(_.target).set(Some(path))
          .lens(_.stamp.installed).set(true))
      }
    }

  private def resolve(c: Content.Plain) = resolverOf(c).resolveAll(c, _: Keyed[(ModState, Mod[F])])

  private def install(rState: RepoState, key: Key) = runTmp[F, RepoState] { tmpDir =>
    val Some((i, mState)) = rState.orderedMods.indexed.find(_._2.key == key)

    for {
      resolved  <- resolve(Content.Component)(mState strengthR mods(key))(tmpDir)
      conflicts =  rState.hasConflicts(_: Path, i)
      changes   <- resolved.foldMapM {
        case (from, Keyed(k, Some(to))) if !conflicts(to) => copy(from, to) as Vector(k -> to)
        case _                                            => Vector.empty[(Key, Path)].point[F]
      }
      sync = modLens(i).modify(setPaths(changes) andThen
        (_.lens(_.stamp.installed).set(true)))
    } yield sync(rState)
  }

  //noinspection ConvertibleToMethodValue
  private def uninstall(key: Key) = for {
    repoState <- state
    modIdx = repoState.orderedMods.indexWhere(_.key == key)
    paths = repoState.orderedMods(modIdx).get.contents.toVector.flatMap(_.target)
    _ <- paths.traverse_(rmTree(_))
  } yield ()
}
