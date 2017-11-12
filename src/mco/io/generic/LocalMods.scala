package mco.io.generic

import scalaz._
import std.vector._
import std.option._
import std.map._
import std.set._
import mco.core.state._
import mco.core._
import mco.data._
import mco.util.syntax.fp._
import mco.util.syntax.any._
import TempFolder._
import Filesystem._
import monocle.function.Index.index


//noinspection ConvertibleToMethodValue
class LocalMods[F[_]: Monad: Filesystem: TempFolder](
  mods: Map[Key, Mod[F]],
  resolver: NameResolver[F]
) extends Mods[F] {
  override def state: F[RepoState] = ???

  override def update(key: Key, diff: Deltas.OfMod) = {
    for {
      st <- state
      target = st.at(key)
    } yield ()
  }

  private def modAt(i: Int) =
    RepoState.orderedMods composeOptional
    index(i) composeLens
    Keyed.lens

  private def componentsIn(
    filter: Key => Boolean)(
    mState: Keyed[ModState])(
    tmpDir: F[Path]) = {
    mods(mState.key)
      .filterProvide {
        case Keyed(key, Content.Component) if filter(key) => true
        case _ => false
      }
      .pipe(f => f(tmpDir))
      .flatMap(resolver.bulk(mState))
  }

  private def runAtFiltered(
    op: (Path, Path) => F[Unit],
    filter: (Path) => Boolean = Function.const(true))(
    targets: Vector[(Path, Keyed[Option[Path]])]
  ) = targets foldMapM {
    case (from, Keyed(k, Some(to))) if filter(to) =>
      op(from, to) as Vector(k -> to)
    case _ =>
      Vector.empty[(Key, Path)].point[F]
  }

  private def install(rState: RepoState, key: Key) = runTmp[F, RepoState] { tmpDir =>
    val (i, mState) = rState.at(key)
    val conflicts = !rState.hasConflicts(_: Path, i)
    for {
      changes <- componentsIn(mState.get.contentEnabled)(mState)(tmpDir) >>=
        runAtFiltered(copy(_, _), conflicts)
      updateState = modAt(i).modify(_.onResolve(changes, installed = true))
    } yield updateState(rState)
  }

  private def uninstall(rState: RepoState, key: Key) = runTmp[F, RepoState] { tmpDir =>
    val (i, mState) = rState.at(key)
    val targets = rState.recoveryIndex(_: Path, i)
    for {
      changes <- componentsIn(mState.get.contentEnabled)(mState)(tmpDir) >>=
        runAtFiltered((_, to) => rmTree(to))
      updateState = modAt(i).modify(_.onResolve(changes, installed = false))
      newState = updateState(rState)
      _ <- changes
        .foldMap {
          case (k, to) => targets(to).foldMap(j => Map(j -> Set(k)))
        }
        .toVector
        .traverse_ { case (j, keys) =>
          componentsIn(keys)(newState.orderedMods(j))(tmpDir)
            .flatMap { runAtFiltered(copy(_, _)) }
            .void
        }
    } yield newState
  }
}
