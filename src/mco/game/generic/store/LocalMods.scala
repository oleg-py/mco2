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
import mco.game.generic._
import mouse.boolean._
import mco.syntax._


//noinspection ConvertibleToMethodValue
class LocalMods[F[_]: Sync: Filesystem: FileStamping](
  contentRoot: Path,
  repoVar: Var[F, RepoState],
  modsVar: Var[F, Map[RelPath, Mod[F]]],
  tryAsMod: Path => F[Option[Mod[F]]],
  computeState: Mod[F] => F[ModState],
  resolver: NameResolver
) extends Mods[F] {
  override def state: F[RepoState] = repoVar()

  override def update(key: RelPath, diff: Deltas.OfMod): F[Unit] = {
    for {
      rState <- state
      (i, (_, modState)) = rState.at(key)
      _ <- if (modState.status == Status.Installed) change(i, modState, Status.Unused)
           else unit.pure[F]
      modState2 <- state.map(_.at(key)._2._2)
      updated = diff.patch(modState2)
      _ <- repoVar ~= RepoState.pathL(key).set(updated)
      _ <- if (updated.status == Status.Installed) change(i, modState, Status.Installed)
           else unit.pure[F]
    } yield ()
  }

  private def change(
    i: Int,
    modState: ModState,
    status: Status
  ) = {
    for {
      repo <- repoVar()
      mods <- modsVar()
      lookupMod = (order: Int) => {
        val (key, _) = repo.orderedMods(order)
        (key, mods(key))
      }
      (key, targetMod) = repo.orderedMods(i)
      conflicts = new Conflicts(repo)
      traversal = new ContentTraversalFS(resolver, repoVar, key)
      installation = new Installation(lookupMod, conflicts, traversal)
      contents = targetMod.contents.collect {
        case (p, cs) if cs.assignedKind == ContentKind.Component => p
      }.toVector
      _ <- installation.alter(i, status, contents).runSync
    } yield ()
  }

  override def remove(key: RelPath): F[Unit] = for {
    _    <- update(key, Deltas.OfMod(status = Some(Status.Installed)))
    _    <- repoVar ~= (_.remove(key))
    path <- modsVar().map(dict => dict(key).backingFile)
    _    <- rmTree(path)
    _    <- modsVar ~= (_ - key)
  } yield ()

  override def liftFile(p: Path): F[Option[ModState]] = {
    val target = contentRoot / p.name
    def tryLift(p: Path) = OptionT(tryAsMod(p))
    def notAlreadyExists(p: Path) = OptionT(exists(p).map(a => (!a).option(unit)))
    def registerMod(mod: Mod[F]) =
      for {
        state <- computeState(mod)
        key   =  rel"${p.name}"
        keyed =  (key, state)
        _     <- repoVar ~= { _ add (keyed, mod.label) }
        _     <- modsVar ~= { _ updated (key, mod) }
      } yield state

    tryLift(p)
      .flatMap(_ => notAlreadyExists(target))
      .semiflatMap(_ => copy(p, target))
      .flatMap(_ => tryLift(target))
      .semiflatMap(registerMod)
      .value
  }
}
