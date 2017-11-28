package mco.io.generic

import scalaz._
import std.vector._
import std.option._
import std.map._
import std.set._

import mco.core._
import mco.core.state._
import mco.core.vars.Var
import mco.data._
import mco.util.syntax.fp._
import mco.util.syntax.any._
import Filesystem._
import mco.data.paths.{Path, RelPath}
import mco.io.state.initMod
import monocle.function.Index.index


//noinspection ConvertibleToMethodValue
class LocalMods[F[_]: Monad: Filesystem](
  contentRoot: Path,
  repoState: Var[F, RepoState],
  mods: Var[F, Map[RelPath, (Path, Mod[F])]],
  tryAsMod: Path => F[Option[Mod[F]]],
  resolver: NameResolver
) extends Mods[F] {
  override def state: F[RepoState] = repoState()

  override def update(key: RelPath, diff: Deltas.OfMod): F[Unit] = {
    val noop = ().point[F]
    for {
      rState <- state
      (i, Keyed(_, modState)) = rState.at(key)
      _ <- if (modState.stamp.enabled) uninstall(key) else noop
      modState2 <- state.map(_.at(key)._2.get)
      updated = diff.patch(modState2)
      _ <- repoState ~= (
        RepoState.orderedMods composeOptional
          index(i) set
          Keyed(key, updated))
      _ <- if (updated.stamp.enabled) install(key) else noop
    } yield ()
  }

  private def modAt(i: Int) =
    RepoState.orderedMods composeOptional
    index(i) composeLens
    Keyed.lens

  private def prepareFiles(
    filter: RelPath => Boolean)(
    index: Int,
    mState: Keyed[ModState]) =
    for {
      dict <- mods()
      provided <- dict(mState.key)._2
        .filterProvide {
          case Keyed(key, Content.Component) if filter(key) => true
          case _ => false
        }
    } yield provided.map(resolver.bulk(index, mState))

  private def filter2[A](f: A => Boolean)(xs: Vector[Keyed[(A, A)]]) =
    xs.filter { case Keyed(_, (_, to)) => f(to) }

  private def runOp[A](op: (A, A) => F[Unit])(xs: Vector[Keyed[(A, A)]]) =
    xs.foldMapM { case Keyed(k, (from, to)) => op(from, to) as Vector(k -> to) }

  private val copyFiles = runOp[Path](copy(_, _)) _
  private val rmFiles = runOp[Path]((_, to) => rmTree(to)) _

  private def install(key: RelPath) =
    for {
      rState <- state
      (i, mState) = rState.at(key)
      conflicts = !rState.hasConflicts(_: Path, i)
      prepared <- prepareFiles(mState.get.contentEnabled)(i, mState)
      changes <- prepared
        .andThen(copyFiles compose filter2(conflicts))
        .runFS
      _ <- repoState ~= modAt(i).modify(_.onResolve(changes, installed = true))
    } yield ()

  private def uninstall(key: RelPath) =
    for {
      rState <- state
      (i, mState) = rState.at(key)
      targets = rState.recoveryIndex(_: Path, i)
      prepared <- prepareFiles(mState.get.contentEnabled)(i, mState)
      changes <- prepared.andThen(rmFiles).runFS
      _ <- repoState ~= modAt(i).modify(_.onResolve(changes, installed = false))
      newState <- state
      _ <- changes
        .foldMap { case (k, to) =>
          targets(to).foldMap(j => Map(j -> Set(k)))
        }
        .toVector
        .traverse_ { case (j, keys) =>
          prepareFiles(keys)(j, newState.orderedMods(j))
            .flatMap(_.andThen(copyFiles).runFS)
            .void
        }
    } yield ()

  override def remove(key: RelPath): F[Unit] = for {
    _ <- update(key, Deltas.OfMod(enabled = Some(false)))
    _ <- repoState ~= (_.remove(key))
    path <- mods().map(dict => dict(key)._1)
    _ <- rmTree(path)
    _ <- mods ~= (_ - key)
  } yield ()

  override def liftFile(p: Path): F[Option[ModState]] = {
    val result = for {
      _ <- OptionT(tryAsMod(p))
      target = contentRoot / p.name
      _ <- OptionT(exists(target).ifM(none[Unit].point[F], some(unit).point[F]))
      _ <- copy(p, target).liftM[OptionT]
      mod <- OptionT(tryAsMod(target))
      state <- initMod(mod).liftM[OptionT]
      key = RelPath(p.name.toString)
      keyed = Keyed(key, state)
      _ <- (repoState ~= { _ add (keyed, mod.label) }).liftM[OptionT]
      _ <- (mods ~= { _ updated (key, (target, mod))}).liftM[OptionT]
    } yield state

    result.run
  }
}
