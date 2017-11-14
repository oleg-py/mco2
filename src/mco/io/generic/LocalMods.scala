package mco.io.generic

import scalaz._
import std.vector._
import std.option._
import std.map._
import std.set._

import mco.core._
import mco.core.state._
import mco.data._
import mco.util.syntax.fp._
import mco.util.syntax.any._
import TempOps._
import Filesystem._
import mco.io.state.initMod
import monocle.function.Index.index
import monocle.macros.Lenses


//noinspection ConvertibleToMethodValue
class LocalMods[F[_]: Monad: TempOps](
  contentRoot: Path,
  repoState: Var[F, RepoState],
  mods: Var[F, Map[Key, (Path, Mod[F])]],
  tryAsMod: Path => F[Option[Mod[F]]],
  resolver: NameResolver[F]
) extends Mods[F] {
  private implicit val filesystemF: Filesystem[F] = TempOps[F].filesystemF

  override def state: F[RepoState] = repoState()

  override def update(key: Key, diff: Deltas.OfMod): F[Unit] = {
    val noop = ().point[F]
    for {
      rState <- state
      (i, Keyed(_, modState)) = rState.at(key)
      _ <- if (modState.stamp.installed) uninstall(key) else noop
      updated = diff.patch(modState)
      _ <- repoState ~= (
        RepoState.orderedMods composeOptional
          index(i) set
          Keyed(key, updated))
      _ <- if (updated.stamp.installed) install(key) else noop
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
      mods()
        .map(dict => dict(mState.key)._2.filterProvide _)
        .map { fn =>
          fn {
            case Keyed(key, Content.Component) if filter(key) => true
            case _ => false
          }
        }
        .flatMap(fn => fn(tmpDir))
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
      rState <- state
      (i, mState) = rState.at(key)
      conflicts = !rState.hasConflicts(_: Path, i)
      changes <- prepareFiles(mState.get.contentEnabled)(mState)(tmpDir)
        .>>= { copyFiles compose filter2(conflicts) compose collapse }
      _ <- repoState ~= modAt(i).modify(_.onResolve(changes, installed = true))
    } yield ()
  }

  private def uninstall(key: Key) = runTmp[F, Unit] { tmpDir =>
    for {
      rState <- state
      (i, mState) = rState.at(key)
      targets = rState.recoveryIndex(_: Path, i)
      changes <- prepareFiles(mState.get.contentEnabled)(mState)(tmpDir) >>= { rmFiles compose collapse }
      _ <- repoState ~= modAt(i).modify(_.onResolve(changes, installed = false))
      newState <- state
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

  override def remove(key: Key) = for {
    _ <- update(key, Deltas.OfMod(installed = Some(false)))
    _ <- repoState ~= (_.remove(key))
    path <- mods().map(dict => dict(key)._1)
    _ <- rmTree(path)
    _ <- mods ~= (_ - key)
  } yield ()

  override def liftFile(p: Path) = {
    val result = for {
      _ <- OptionT(tryAsMod(p))
      target = contentRoot / p.name
      _ <- exists(target).ifM(none.point[F], some(()).point[F]).pipe(OptionT(_))
      _ <- copy(p, target).liftM[OptionT]
      mod <- OptionT(tryAsMod(target))
      state <- runTmp[F, ModState](initMod(mod)).liftM[OptionT]
      key = Key(p.name)
      keyed = Keyed(key, state)
      _ <- (repoState ~= { _ add keyed }).liftM[OptionT]
      _ <- (mods ~= { _ updated (key, (target, mod))}).liftM[OptionT]
    } yield state

    result.run
  }
}