package mco.game.generic.store

import scalaz._
import scalaz.std.option._

import mco.core._
import mco.core.paths._
import mco.core.state._
import mco.core.vars.Var
import mco.io.Filesystem, Filesystem._
import mco.io.state.initMod
import mco.util.syntax.any._
import mco.util.syntax.fp._
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
      (i, Pointed(_, modState)) = rState.at(key)
      _ <- if (modState.stamp.enabled) uninstall(i, modState) else noop
      modState2 <- state.map(_.at(key)._2.get)
      updated = diff.patch(modState2)
      _ <- repoState ~= (
        RepoState.orderedMods composeOptional
          index(i) set
          Pointed(key, updated))
      _ <- if (updated.stamp.enabled) install(i, modState) else noop
    } yield ()
  }

  def modMap = mods().map(_.mapValues(_._2))

  def install(i: Int, modState: ModState) = {
    val content = modState.contents.keySet
    modMap.flatMap {
      new InstallFocus[F](repoState, _, resolver, i).install(content)
    }
  }

  def uninstall(i: Int, modState: ModState) = {
    val content = modState.contents.keySet
    modMap.flatMap {
      new InstallFocus[F](repoState, _, resolver, i).remove(content)
    }
  }

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
      keyed = Pointed(key, state)
      _ <- (repoState ~= { _ add (keyed, mod.label) }).liftM[OptionT]
      _ <- (mods ~= { _ updated (key, (target, mod))}).liftM[OptionT]
    } yield state

    result.run
  }
}
