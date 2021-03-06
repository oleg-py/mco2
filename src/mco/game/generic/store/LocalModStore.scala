package mco.game.generic.store

import cats.syntax.all._
import cats.instances.unit._
import mco.core._
import mco.core.paths._
import mco.core.state._
import mco.core.vars.Var
import mco.io.{FileStamping, Filesystem}
import Filesystem._
import cats.effect.Sync
import mco.game.generic._
import mco.game.generic.extractors.Extractor
import mco.syntax._


//noinspection ConvertibleToMethodValue
class LocalModStore[F[_]: Sync: Filesystem: FileStamping](
  contentRoot: Path,
  repoVar: Var[F, RepoState],
  modsVar: Var[F, Map[RelPath, Mod[F]]],
  mkExtractor: Path => Extractor[F],
  computeState: Mod[F] => F[ModState],
  resolver: NameResolver
) extends ModStore[F] {
  override def state: F[RepoState] = repoVar()

  override def update(key: RelPath, diff: Deltas.OfMod): F[Unit] = {
    for {
      rState <- state
      (i, (_, modState)) = rState.at(key)
      _ <- ifM (modState.status == Status.Installed) {
        change(i, modState, Status.Unused)
      }
      modState2 <- state.map(_.at(key)._2._2)
      updated = diff.patch(modState2)
      _ <- repoVar ~= RepoState.pathL(key).set(updated)
      _ <- ifM (updated.status == Status.Installed) {
        change(i, modState, Status.Installed)
      }
    } yield ()
  }

  private def change(i: Int, modState: ModState, status: Status) = {
    for {
      repo <- repoVar()
      mods <- modsVar()
      lookupMod = (order: Int) => {
        val (key, _) = repo.orderedMods(order)
        (key, mods(key).data)
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

  override def liftFile(p: Path): F[ModState] = {
    val target = contentRoot / p.name
    for {
      exist <- exists(target)
      _     <- ifM (exist) {
        Sync[F].raiseError[Unit](new Exception(s"File already exists: $target"))
      }
      _     <- copy(p, target)
      mod   =  Mod(target, mkExtractor(target))
      state <- computeState(mod)
      key   =  rel"${p.name}"
      keyed =  (key, state)
      _     <- repoVar ~= { _ add (keyed, mod.label) }
      _     <- modsVar ~= { _ updated (key, mod) }
    } yield state
  }
}
