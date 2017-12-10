package mco.game.generic.store

import cats._
import cats.syntax.all._
import mco.core._
import mco.core.paths._
import mco.core.state._
import mco.core.vars.Var
import mco.io.{FileStamping, Filesystem}
import Filesystem._
import cats.data.OptionT
import cats.effect.Sync
import mouse.boolean._
import mco.syntax._
import monocle.function.Index.index


//noinspection ConvertibleToMethodValue
class LocalMods[F[_]: Sync: Filesystem: FileStamping](
  contentRoot: Path,
  repoState: Var[F, RepoState],
  modsState: Var[F, Map[RelPath, Mod[F]]],
  tryAsMod: Path => F[Option[Mod[F]]],
  computeState: Mod[F] => F[ModState],
  resolver: NameResolver
) extends Mods[F] {
  override def state: F[RepoState] = repoState()

  override def update(key: RelPath, diff: Deltas.OfMod): F[Unit] = {
    val noop = ().pure[F]
    for {
      rState <- state
      (i, Pointed(_, modState)) = rState.at(key)
      _ <- if (modState.stamp.enabled) change(i, modState, _.remove) else noop
      modState2 <- state.map(_.at(key)._2.get)
      updated = diff.patch(modState2)
      _ <- repoState ~= (
        RepoState.orderedMods composeOptional
          index(i) set
          Pointed(key, updated))
      _ <- if (updated.stamp.enabled) change(i, modState, _.install) else noop
    } yield ()
  }

  private def change(
    i: Int,
    modState: ModState,
    f: InstallFocus[F] => Set[RelPath] => F[Unit]) =
    for {
      mods     <- modsState()
      contents =  modState.contents.keySet
      focus    =  new InstallFocus(repoState, mods, resolver, i)
      _        <- f(focus)(contents)
    } yield ()

  override def remove(key: RelPath): F[Unit] = for {
    _    <- update(key, Deltas.OfMod(enabled = Some(false)))
    _    <- repoState ~= (_.remove(key))
    path <- modsState().map(dict => dict(key).backingFile)
    _    <- rmTree(path)
    _    <- modsState ~= (_ - key)
  } yield ()

  override def liftFile(p: Path): F[Option[ModState]] = {
    val target = contentRoot / p.name
    def tryLift(p: Path) = OptionT(tryAsMod(p))
    def notAlreadyExists(p: Path) = OptionT(exists(p).map(a => (!a).option(unit)))
    def registerMod(mod: Mod[F]) =
      for {
        state <- computeState(mod)
        key   =  rel"${p.name}"
        keyed =  Pointed(key, state)
        _     <- repoState ~= { _ add (keyed, mod.label) }
        _     <- modsState ~= { _ updated (key, mod) }
      } yield state

    tryLift(p)
      .flatMap(_ => notAlreadyExists(target))
      .semiflatMap(_ => copy(p, target))
      .flatMap(_ => tryLift(target))
      .semiflatMap(registerMod)
      .value
  }
}
