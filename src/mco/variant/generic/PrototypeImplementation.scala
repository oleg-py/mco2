package mco.variant.generic

import scalaz._
import std.vector._

import mco.core._
import mco.core.state.RepoState
import mco.core.vars._
import mco.data.paths._
import mco.data.Keyed
import mco.io.generic._
import mco.io.generic.Filesystem._
import mco.io.state.initMod
import mco.stubs.{LoggingFilesystem, VarFilesystem}
import mco.stubs.cells._
import mco.util.syntax.??
import mco.util.syntax.fp._


//noinspection ConvertibleToMethodValue
object PrototypeImplementation {
  private val toKey = (p: Path) => RelPath(p.name.toString)
  type MThrow[F[_]] = MonadError[F, Throwable]

  private def filesystem[F[_]: Capture: MThrow](
    config: GenericConfig,
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

    config.repo.paths
      .traverse(determineFS)
      .map { case Vector(source, images, target) =>
        new LoggingFilesystem(new VirtualizedFilesystem[F](
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
        initMod[F](mod).map(Keyed(toKey(path), _))
      }
      modMap = mods.map { case t @ (path, _) => toKey(path) -> t }.toMap
      labels = modMap.map { case (key, (_, mod)) => key -> mod.label }
      repoVar = new MutableVar(RepoState(orderedMods, labels))
    } yield new LocalMods(
      path"-source",
      repoVar,
      new MutableVar(modMap),
      typer(_),
      NameResolver.subdirs(path"-target")
    )
  }.widen

  def algebras[F[_]: Capture: MThrow](
    config: GenericConfig,
    cwd: Path
  ): F[(Mods[F], ImageStore[F])] =
    filesystem(config, cwd) >>= { case (fs, arch) =>
      implicit val filesystem: Filesystem[F] = fs
      implicit val archiving: Archiving[F] = arch
      mods tuple images(config.files.isImage)
    }
}
