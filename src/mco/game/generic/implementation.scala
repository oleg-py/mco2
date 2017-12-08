package mco.game.generic

import cats._
import cats.implicits._
import mco.core._
import mco.core.paths._
import mco.core.state.RepoState
import mco.core.vars._
import mco.game.generic.store.{LocalImageStore, LocalMods, SimpleModTypes}
import mco.io.impls._
import mco.io.state.initMod
import mco.io.Filesystem
import Filesystem._
import cats.effect.Sync
import mco.stubs.cells._
import mco.stubs.{LoggingFilesystem, VarFilesystem}
import mco.util.syntax.??


//noinspection ConvertibleToMethodValue
object implementation {
  private val toKey = (p: Path) => RelPath(p.name.toString)
  type MThrow[F[_]] = MonadError[F, Throwable]

  private def filesystem[F[_]: Sync](
    config: StoreConfig.Repo,
    cwd: Path
  ): F[Filesystem[F]] = {
    val localFS:  Filesystem[F] = new LocalFilesystem[F]

    def determineFS(subpath: String) =
      if (subpath startsWith "varfs!") {
        val file = rel"${subpath.drop("varfs!".length)}"
        val backend = CacheVar(dir().pure[F])(
          new JavaSerializableVar(cwd / file)(??, localFS),
          new MutableVar(_).pure[F].widen
        ).map(new VarFilesystem(_): Filesystem[F])
        backend.tupleLeft(Path.root)
      } else {
        val target = path"$cwd/$subpath"
        localFS.ensureDir(target).as((target, localFS))
      }

    config.paths
      .traverse(determineFS)
      .map { case Vector(source, images, target) =>
        new LoggingFilesystem(new VirtualRootsFilesystem[F](
          Map(
            (seg"-source", source),
            (seg"-target", target),
            (seg"-images", images),
            (seg"-os", (Path.root, localFS))
          ),
          seg"-os",
          localFS
        )(??, Eq.fromUniversalEquals))
      }
      .widen
  }

  private def images[F[_]: Filesystem: Sync](
    isImage: Segment => Boolean
  ): F[ImageStore[F]] = {
    CacheVar(Map.empty[RelPath, RelPath].pure[F])(
      new JavaSerializableVar(path"-target/.imgdb"),
      new MutableVar(_).pure[F].widen
    )
      .map(new LocalImageStore(path"-images", _, isImage))
      .widen
  }

  private def mods[F[_]: Filesystem: Sync]: F[Mods[F]] = {
    for {
      _ <- ensureDir(path"-target")
      typer = new SimpleModTypes
      mods <- typer.allIn(path"-source")
      orderedMods <- mods.traverse { case (path, mod) =>
        initMod[F](mod).map(Pointed(toKey(path), _))
      }
      modMap = mods.map { case t @ (path, _) => toKey(path) -> t }.toMap
      labels = modMap.map { case (key, (_, mod)) => key -> mod.label }
      repoVar = new MutableVar(RepoState(orderedMods, labels))
      stamping = new FilesystemVarStamping[F](new MutableVar(Map()))
    } yield new LocalMods(
      path"-source",
      repoVar,
      new MutableVar(modMap),
      typer(_),
      NameResolver.overrides(path"-target")
    )(??, ??, stamping)
  }.widen

  def algebras[F[_]: Sync](
    config: StoreConfig,
    cwd: Path
  ): F[Vector[(String, Mods[F], ImageStore[F])]] =
    config.repos.toVector.traverse { case (_, repo) =>
      filesystem(repo, cwd) >>= { implicit fs =>
        (mods, images(config.files.isImage)).mapN { (m, i) =>
          (repo.title, m, i)
        }
      }
    }
}
