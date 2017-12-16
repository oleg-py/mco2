package mco.game.generic

import cats._
import cats.implicits._
import mco.core._
import mco.core.paths._
import mco.core.state.{ModState, RepoState}
import mco.core.vars._
import mco.game.generic.store._
import mco.io.impls._
import mco.io.{FileStamping, Filesystem}
import Filesystem._
import cats.effect.Sync
import mco.stubs.cells._
import mco.stubs.VarFilesystem
import mco.syntax.??
import monix.execution.atomic.{Atomic, AtomicBuilder}


//noinspection ConvertibleToMethodValue
object implementation {
  private val (target, source) = (path"-target", path"-source")

  private def filesystem[F[_]: Sync](
    config: StoreConfig.Repo,
    cwd: Path
  ): F[Filesystem[F]] = {
    val localFS: Filesystem[F] = new LocalFilesystem[F]

    def determineFS(subpath: String) = {
      val target = if (subpath startsWith "./") path"$cwd/$subpath"
                   else path"$subpath"
      localFS.ensureDir(target).as((target, localFS))
    }

    config.paths
      .traverse(determineFS)
      .map { case Vector(sourceDir, imagesDir, targetDir) =>
        new VirtualRootsFilesystem[F](
          Map(
            (seg"-source", sourceDir),
            (seg"-target", targetDir),
            (seg"-images", imagesDir),
            (seg"-os", (Path.root, localFS))
          ),
          seg"-os",
          localFS,
          Eq.instance(_ eq _) // use ref equality to compare filesystems
        )
      }
      .widen
  }

  private def images[F[_]: Filesystem: Sync](
    isImage: Vector[String]
  ): F[ImageStore[F]] = {
    new LocalImageStore(path"-images", isImage).pure[F].widen
  }

  private def miniDB[F[_]: Sync: Filesystem, A](
    file: RelPath,
    initial: A,
    prefix: Path = target
  )(implicit
    b: AtomicBuilder[A, _ <: Atomic[A]]
  ): F[Var[F, A]] =
    CacheVar(initial.pure[F])(
      new JavaSerializableVar(prefix / file),
      new MutableVar(_).pure[F].widen
    )

  private def getStates[F[_]: Sync: Filesystem: FileStamping](
    resolver: NameResolver,
    mods: Vector[Mod[F]]
  ): F[(Var[F, RepoState], ModStates[F])] = {
    val kindOf = (_: RelPath) => ContentKind.Component

    for {
      repoStateDb <- miniDB(rel".state", RepoState())
      lastSaved   <- repoStateDb()
      states      =  new ModStates[F](source, resolver, kindOf, lastSaved.orderedMods)
      nextState   <- states.computeAll(mods)
      _           <- repoStateDb := nextState
    } yield (repoStateDb, states)
  }

  private def mods[F[_]: Filesystem: Sync]: F[Mods[F]] = {
    val resolver = NameResolver.overrides(target)
    val typer    = new SimpleModTypes[F]
    miniDB(rel".stamp", FilesystemVarStamping.defaultState)
      .map(new FilesystemVarStamping[F](_))
      .flatMap { implicit stamping =>
        for {
          existing <- typer.allIn(source)
          statesR  <- getStates(resolver, existing)

          (repoState, modStates) = statesR
          modsMap = existing.map { m => (m.backingFile.relTo(source), m) }.toMap

        } yield new LocalMods(
          source, repoState, new MutableVar(modsMap), typer.apply,
          modStates.computeState, resolver
        )
      }
  }.widen

  def algebras[F[_]: Sync](
    config: StoreConfig,
    cwd: Path
  ): F[Vector[(String, Mods[F], ImageStore[F])]] =
    config.repos.toVector.traverse { case (_, repo) =>
      filesystem(repo, cwd) >>= { implicit fs =>
        (mods, images(config.files.images)).mapN { (m, i) =>
          (repo.title, m, i)
        }
      }
    }
}
