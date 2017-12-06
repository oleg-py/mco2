package mco.game.generic.store

import scalaz._
import std.vector._
import syntax.std.boolean._

import mco.core.paths.{Keyed, Path, RelPath}
import mco.core.state._
import mco.core.vars.Var
import mco.core.{Mod, NameResolver}
import mco.core.paths._
import mco.io.{Filesystem, TempOp}
import mco.util.syntax.any._
import mco.util.syntax.fp._


class InstallFocus[F[_]: Monad: Filesystem](
  repoState: Var[F, RepoState],
  mods: Map[RelPath, Mod[F]],
  resolver: NameResolver,
  modFocus: Int
) {
  def refocus(i: Int) = new InstallFocus(repoState, mods, resolver, i)

  private val currentModState = repoState.xmapF[Keyed[ModState]](
    rs => rs.orderedMods(modFocus).point[F],
    kms => for {
      rs  <- repoState()
    } yield rs.copy(orderedMods = rs.orderedMods.updated(modFocus, kms))
  )

  private def getResolveFn: F[RelPath => Path] =
    for (mod <- currentModState()) yield resolver(mod, _: RelPath)

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

  private class ContentOp(key: RelPath, from: Path) {
    val currentContent = currentModState.xmapF[ContentState](
      kms => kms.get.contents.lookup(key).getOrElse(ContentState(mzero[Stamp])).point[F],
      cs => for {
        kms <- currentModState()
        map =  kms.get.contents.insert(key, cs)
      } yield kms.map(_.copy(contents = map))
    )

    def run(copy: Boolean) =
      for {
        rState  <- repoState()
        resolve <- getResolveFn
        path    =  resolve(key)
        conflict=  rState.hasConflicts(path, modFocus)
        _       <- currentContent ~= ContentState.target.set(copy.option(path))
        _       <- currentContent ~= ContentState.stamp.modify(Stamp.installed.set(copy))
        _       <- if (conflict) unit.point[F]
                   else if (copy) Filesystem.copy(from, path)
                   else Filesystem.rmIfExists(path)
      } yield ()
  }

  private def copyOrRemoveFiles(copy: Boolean)(op: TempOp[F, Vector[Keyed[Path]]]) =
    op.andThen(_.traverse_ { case Keyed(key, from) =>
        new ContentOp(key, from).run(copy)
    }).runFS

  private def copyFiles = copyOrRemoveFiles(copy = true) _
  private def removeFiles(p: Vector[RelPath]) =
    copyOrRemoveFiles(copy = false)(TempOp(p.map(Keyed(_, path"")).point[F]))

  private def runOp(copy: Boolean)(only: ISet[RelPath]): F[Unit] =
    for {
      state     <- currentModState()
      mod       =  mods(state.key)
      list      <- mod.list
      filtered  =  list.filter(only.contains)
      _         <- if (copy) copyFiles(mod.provideVec(filtered))
                   else removeFiles(filtered)
      func      =  if (copy) overridesOf
                   else underridesOf
      conflicts <- func(filtered)
      _         <- conflicts.traverse_ { case (relPath, i) =>
        refocus(i).runOp(copy = true)(ISet.singleton(relPath))
      }
      _         <- currentModState ~= { s =>
        s.map(_.copy(s.get.stamp.copy(installed = copy)))
      }
    } yield ()

  val install = runOp(copy = true) _
  val remove = runOp(copy = false) _
}
