package mco.game.generic

import scalaz._
import std.vector._
import std.map._

import mco.core._
import mco.core.paths._
import mco.core.state.RepoState
import mco.core.vars._
import mco.game.generic.store.{LocalImageStore, LocalMods, SimpleModTypes}
import mco.io.impls._
import mco.io.state.initMod
import mco.io.{Archiving, Filesystem}, Filesystem._
import mco.stubs.cells._
import mco.stubs.{LoggingFilesystem, VarFilesystem}
import mco.util.syntax.??
import mco.util.syntax.fp._


//noinspection ConvertibleToMethodValue
object implementation {
  private val toKey = (p: Path) => RelPath(p.name.toString)
  type MThrow[F[_]] = MonadError[F, Throwable]

  private def filesystem[F[_]: Capture: MThrow](
    config: StoreConfig.Repo,
    cwd: Path
  ): F[(Filesystem[F], Archiving[F])] = {
    val localFS:  Filesystem[F] = new LocalFilesystem[F]

    def determineFS(subpath: String) =
      if (subpath startsWith "varfs!") {
        val file = rel"${subpath.drop("varfs!".length)}"
        val backend = CacheVar(dir().point[F])(
          new JavaSerializableVar(cwd / file)(??, ??, localFS),
          new MutableVar(_).point[F].widen
        ).map(new VarFilesystem(_): Filesystem[F])
        backend.strengthL(Path.root)
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
        )(??, Equal.equalRef))
      }
      .fproduct(_.archiving)
      .widen
  }

  private def images[F[_]: Filesystem: Capture: MThrow](
    isImage: Segment => Boolean
  ): F[ImageStore[F]] = {
    CacheVar(IMap.empty[RelPath, RelPath].point[F])(
      new JavaSerializableVar(path"-target/.imgdb"),
      new MutableVar(_).point[F].widen
    )
      .map(new LocalImageStore(path"-images", _, isImage))
      .widen
  }

  private def mods[F[_]: Filesystem: Capture: Archiving: MThrow]: F[Mods[F]] = {
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
    } yield new LocalMods(
      path"-source",
      repoVar,
      new MutableVar(modMap),
      typer(_),
      NameResolver.overrides(path"-target")
    )
  }.widen

  def algebras[F[_]: Capture: MThrow](
    config: StoreConfig,
    cwd: Path
  ): F[Vector[(String, Mods[F], ImageStore[F])]] =
    config.repos.toVector.traverse { case (_, repo) =>
      filesystem(repo, cwd) >>= { case (fs, arch) =>
        implicit val filesystem: Filesystem[F] = fs
        implicit val archiving: Archiving[F] = arch

        (mods |@| images(config.files.isImage)) { (m, i) =>
          (repo.title, m, i)
        }
      }
    }
}
