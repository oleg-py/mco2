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
class LocalMods[F[_]: MonadState[?, RepoState]: Filesystem: TempFolder](
  mods: Map[Key, Mod[F]],
  resolver: NameResolver[F]
) extends Mods[F] {
  private val stateM = MonadState[F, RepoState]
  override def state: F[RepoState] = stateM.get

  override def update(key: Key, diff: Deltas.OfMod): F[Unit] = {
    val unitF = ().point[F]
    for {
      rState <- stateM.get
      (i, Keyed(_, modState)) = rState.at(key)
      _ <- if (modState.stamp.installed) uninstall(key) else unitF
      updated = diff.patch(modState)
      _ <- stateM.modify(RepoState.orderedMods composeOptional index(i) set Keyed(key, updated))
      _ <- if (updated.stamp.installed) unitF else install(key)
    } yield ()
  }

  private def modAt(i: Int) =
    RepoState.orderedMods composeOptional
    index(i) composeLens
    Keyed.lens

  private def prepareFiles(
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

  private def collapse[A, B](xs: Vector[(A, Keyed[Option[B]])]): Vector[Keyed[(A, B)]] =
    xs.collect { case (from, Keyed(k, Some(to))) => Keyed(k, from -> to) }

  private def filter2[A](f: A => Boolean)(xs: Vector[Keyed[(A, A)]]) =
    xs.filter { case Keyed(_, (_, to)) => f(to) }

  private def runOp[A](op: (A, A) => F[Unit])(xs: Vector[Keyed[(A, A)]]) =
    xs.foldMapM { case Keyed(k, (from, to)) => op(from, to) as Vector(k -> to) }

  private val copyFiles = runOp[Path](copy(_, _)) _
  private val rmFiles = runOp[Path]((_, to) => rmTree(to)) _

  private def install(key: Key) = runTmp[F, Unit] { tmpDir =>
    for {
      rState <- stateM.get
      (i, mState) = rState.at(key)
      conflicts = !rState.hasConflicts(_: Path, i)
      changes <- prepareFiles(mState.get.contentEnabled)(mState)(tmpDir)
        .>>= { copyFiles compose filter2(conflicts) compose collapse }
      _ <- stateM.modify(modAt(i).modify(_.onResolve(changes, installed = true)))
    } yield ()
  }

  private def uninstall(key: Key) = runTmp[F, Unit] { tmpDir =>
    for {
      rState <- stateM.get
      (i, mState) = rState.at(key)
      targets = rState.recoveryIndex(_: Path, i)
      changes <- prepareFiles(mState.get.contentEnabled)(mState)(tmpDir) >>= { rmFiles compose collapse }
      newState = modAt(i).modify(_.onResolve(changes, installed = false))(rState)
      _ <- stateM.put(newState)
      _ <- changes
        .foldMap { case (k, to) =>
          targets(to).foldMap(j => Map(j -> Set(k)))
        }
        .toVector
        .traverse_ { case (j, keys) =>
          prepareFiles(keys)(newState.orderedMods(j))(tmpDir)
            .flatMap { copyFiles compose collapse }
            .void
        }
    } yield ()
  }
}
