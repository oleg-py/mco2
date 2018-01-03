package mco.game.generic

import cats._
import cats.implicits._
import mco.core._
import mco.core.paths._
import mco.core.state.RepoState
import mco.core.vars._
import mco.game.generic.store._
import mco.io.impls._
import mco.io.{FileStamping, Filesystem}
import cats.effect.Sync
import monix.execution.atomic.{Atomic, AtomicBuilder}
import boopickle.Default._
import mco.game.generic.extractors._
import mco.game.sims3.S3CE
import mco.game.sims3.extractors.Sims3PackExtractor

//noinspection ConvertibleToMethodValue
object implementation {
  private def miniDB[F[_]: Sync: Filesystem, A: Pickler](
    file: Path,
    initial: A
  )(implicit
    b: AtomicBuilder[A, _ <: Atomic[A]]
  ): F[Var[F, A]] =
    CacheVar(initial.pure[F])(
      new BoopickleVar(file),
      MutableVar(_)
    )

  private def getStates[F[_]: Sync: Filesystem: FileStamping](
    dbRoot: Path,
    source: Path,
    resolver: NameResolver,
    mods: Vector[Mod[F]]
  ): F[(Var[F, RepoState], ModStates[F])] = {
    val kindOf = (_: RelPath) => ContentKind.Component

    for {
      repoStateDb <- miniDB(dbRoot / seg".state", RepoState())
      lastSaved   <- repoStateDb()
      states      =  new ModStates[F](source, resolver, kindOf, lastSaved.orderedMods)
      nextState   <- states.computeAll(mods)
      _           <- repoStateDb := nextState
    } yield (repoStateDb, states)
  }

  def joinPath(cwd: Path, inner: String) =
    if (inner startsWith "./") path"$cwd/$inner"
    else path"$inner"

  def mkStamping[F[_]: Filesystem: Sync](dbRoot: Path): F[FileStamping[F]] =
    miniDB(dbRoot / seg".stamp", FilesystemVarStamping.defaultState)
      .map(new FilesystemVarStamping[F](_))
      .widen

  private def mkRepoAlgebras[F[_]: Sync: Filesystem](
    cwd: Path,
    root: StoreConfig,
    repo: StoreConfig.Repo
  ) = {
    implicit val s3ce = new S3CE[F](joinPath(cwd, root.tools.s3ce))
    implicit val archiving = new SevenZipArchiving[F]
    val mkExtractor = Extractor.deep(
      FolderExtractor[F],
      ArchiveExtractor[F],
      CachedExtractor[F](cwd / seg".cache", Sims3PackExtractor[F])
    )
    val source = joinPath(cwd, repo.mods)
    val target = joinPath(cwd, repo.target)
    val resolver = NameResolver.flatten(target)
    val images = new LocalImageStore(joinPath(cwd, repo.images), root.files.images)
    mkStamping[F](joinPath(cwd, repo.target)).flatMap { implicit stamping =>
      for {
        files    <- Filesystem.childrenOf(source)
        existing =  files.map(p => Mod(p, mkExtractor(p))).toVector
        states   <- getStates(target, source, resolver, existing)

        modsMap = existing
          .map { mod => (mod.backingFile.relTo(source), mod) }
          .toMap

        state <- MutableVar(modsMap)
      } yield {
        val (repoState, modStates) = states

        val localMods = new LocalModStore[F](
          source,
          repoState,
          state,
          mkExtractor,
          modStates.computeState,
          resolver
        )
        (repo.title, localMods, images)
      }
    }
  }

  def apply[F[_]: Sync](
    config: StoreConfig,
    cwd: Path
  ): F[Vector[(String, ModStore[F], ImageStore[F])]] = {
    implicit val filesystem: Filesystem[F] = new LocalFilesystem[F]
    config.repos.values.toVector
      .traverse(mkRepoAlgebras(cwd, config, _))
      .widen
  }
}
