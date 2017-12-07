package mco.game.generic.store

import scalaz._
import std.vector._
import syntax.std.boolean._

import mco.core.paths.{Path, Pointed, RelPath}
import mco.core.state._
import mco.core.vars.Var
import mco.core.{Mod, NameResolver}
import mco.core.paths._
import mco.io.{FileStamping, Filesystem, InTemp}
import mco.util.syntax.any._
import mco.util.syntax.fp._


class InstallFocus[F[_]: Monad: Filesystem: FileStamping](
  repoState: Var[F, RepoState],
  mods: Map[RelPath, Mod[F]],
  resolver: NameResolver,
  modFocus: Int
) {
  def refocus(i: Int) = new InstallFocus(repoState, mods, resolver, i)

  private val currentModState = repoState.xmapF[Pointed[ModState]](
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

  private def currentMod = currentModState().map(s => mods(s.key))

  private class ContentOp(key: RelPath, from: Path) {
    val currentContent = currentModState.xmapF[ContentState](
      kms => kms.get.contents.lookup(key).getOrElse(ContentState(mzero[Stamp])).point[F],
      cs => for {
        kms <- currentModState()
        map =  kms.get.contents.insert(key, cs)
      } yield kms.map(_.copy(contents = map))
    )

    def innerPathF: F[InnerPath] = currentMod.map(_.backingFile).strengthR(key)

    def run(copy: Boolean) =
      for {
        rState  <- repoState()
        resolve <- getResolveFn
        path    =  resolve(key)
        inner   <- innerPathF
        conflict=  rState.hasConflicts(path, modFocus)
        noNeed  <- FileStamping.likelySame(inner, from, path) // when from is empty, noop
        _       <- currentContent ~= ContentState.target.set(copy.option(path))
        _       <- currentContent ~= ContentState.stamp.modify(Stamp.installed.set(copy))
        _       <- if (conflict || noNeed) unit.point[F]
                   else if (copy) Filesystem.copy(from, path) >> FileStamping.overwrite(inner, path)
                   else Filesystem.rmIfExists(path) >> FileStamping.overwrite(inner, path)
      } yield ()
  }

  private def copyOrRemoveFiles(copy: Boolean)(op: InTemp[F, Vector[Pointed[Path]]]) =
    op
      .andThen(_.traverse_ { p => new ContentOp(p.key, p.get).run(copy) })
      .runFS

  private def copyFiles = copyOrRemoveFiles(copy = true) _
  private def removeFiles(p: Vector[RelPath]) =
    copyOrRemoveFiles(copy = false)(InTemp(p.map(Pointed(_, path"")).point[F]))

  private def runOp(copy: Boolean)(only: ISet[RelPath]): F[Unit] =
    for {
      mod       <- currentMod
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
