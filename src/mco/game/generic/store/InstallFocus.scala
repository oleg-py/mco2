package mco.game.generic.store

import cats._
import cats.effect.Sync
import cats.instances.vector._
import cats.syntax.all._
import mouse.boolean._
import mco.core.paths.{Path, Pointed, RelPath}
import mco.core.state._
import mco.core.vars.Var
import mco.core.{Mod, NameResolver}
import mco.core.paths._
import mco.io.{FileStamping, Filesystem}
import mco.util.syntax.any._


class InstallFocus[F[_]: Sync: Filesystem: FileStamping](
  repoState: Var[F, RepoState],
  mods: Map[RelPath, Mod[F]],
  resolver: NameResolver,
  modFocus: Int
) {
  def refocus(i: Int) = new InstallFocus(repoState, mods, resolver, i)

  private val currentModState = repoState.xmapF[Pointed[ModState]](
    rs => rs.orderedMods(modFocus).pure[F],
    kms => for {
      rs  <- repoState()
    } yield rs.copy(orderedMods = rs.orderedMods.updated(modFocus, kms))
  )

  private def getResolveFn: F[RelPath => Path] =
    for (mod <- currentModState()) yield resolver(mod.key, _: RelPath)

  private def getConflicts[A](f: (RepoState, Path, Int) => Option[A])(
    list: Vector[RelPath]
  ): F[Vector[(RelPath, A)]] =
    for {
      state   <- repoState()
      resolve <- getResolveFn
    } yield for {
      rel <- list
      idx <- f(state, resolve(rel), modFocus)
    } yield (rel, idx)

  private def overridesOf = getConflicts(_.overrideIndex(_, _)) _
  private def underridesOf = getConflicts(_.recoveryIndex(_, _)) _

  private def currentMod = currentModState().map(s => mods(s.key))

  private class ContentOp(key: RelPath, from: Path) {
    val currentContent = currentModState.xmapF[ContentState](
      kms => kms.get.contents.getOrElse(key, ContentState(Monoid[Stamp].empty)).pure[F],
      cs => for {
        kms <- currentModState()
        map =  kms.get.contents.updated(key, cs)
      } yield kms.map(_.copy(contents = map))
    )

    def innerPathF: F[InnerPath] = currentMod.map(_.backingFile).tupleRight(key)

    def run(copy: Boolean) =
      for {
        rState  <- repoState()
        resolve <- getResolveFn
        path    =  resolve(key)
        inner   <- innerPathF
        conflict=  rState.hasConflicts(path, modFocus)
        noNeed  <- if (copy) FileStamping.likelySame(inner, from, path)
                   else false.pure[F]
        _       <- currentContent ~= ContentState.target.set(copy.option(path))
        _       <- currentContent ~= ContentState.stamp.modify(Stamp.installed.set(copy))
        _       <- if (conflict || noNeed) unit.pure[F]
                   else if (copy) Filesystem.copy(from, path) *> FileStamping.overwrite(inner, path)
                   else Filesystem.rmIfExists(path) *> FileStamping.overwrite(inner, path)
      } yield ()
  }

  private def copyOrRemoveFiles(copy: Boolean)(op: fs2.Stream[F, Pointed[Path]]) =
    op.evalMap(p => new ContentOp(p.key, p.get).run(copy)).runSync

  private def copyFiles = copyOrRemoveFiles(copy = true) _
  private def removeFiles(p: Vector[RelPath]) =
    copyOrRemoveFiles(copy = false)(fs2.Stream.emits(p.map(Pointed(_, path""))).covary)

  private def runOp(copy: Boolean)(only: Set[RelPath]): F[Unit] =
    for {
      mod       <- currentMod
      list      <- mod.list
      filtered  =  list.filter(only.contains)
      _         <- if (copy) copyFiles(mod.provide(filtered))
                   else removeFiles(filtered)
      func      =  if (copy) overridesOf
                   else underridesOf
      conflicts <- func(filtered)
      _         <- conflicts.traverse_ { case (relPath, i) =>
        refocus(i).runOp(copy = true)(Set(relPath))
      }
      _         <- currentModState ~= { s =>
        s.map(_.copy(s.get.stamp.copy(installed = copy)))
      }
    } yield ()

  val install = runOp(copy = true) _
  val remove = runOp(copy = false) _
}
