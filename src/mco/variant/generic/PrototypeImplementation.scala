package mco.variant.generic

import scalaz._
import std.vector._

import mco.core.Mods
import mco.core.state.RepoState
import mco.core.vars._
import mco.data.{Key, Keyed, Path}
import mco.io.generic._
import mco.io.generic.Filesystem._
import mco.io.state.initMod
import mco.stubs.{Cell, LoggingFilesystem, VarFilesystem}
import mco.util.Capture
import mco.util.syntax.??
import mco.util.syntax.fp._


//noinspection ConvertibleToMethodValue
object PrototypeImplementation {

  def algebra[F[_]: Capture: MonadError[?[_], Throwable]](
    config: GenericConfig,
    root: Path
  ): F[Mods[F]] = {
    val toKey = (p: Path) => Key(p.name)

    val localFS = new LocalFilesystem[F]

    def determineFS(subpath: String) =
      if (subpath startsWith "varfs!") {
        val file = subpath.drop("varfs!".length)
        val backend = CacheVar(Cell.Dir().point[F])(
          new JavaSerializableVar(root / file)(??, ??, localFS),
          new MutableVar(_).point[F].widen
        )
          .map(new PrintingVar(_)) // TODO - remove logging here
          .map(new VarFilesystem(_))
          .widen[Filesystem[F]]
        backend.strengthL(Path.root)
      } else {
        localFS.ensureDir(root / subpath).as(
          (root / subpath, localFS: Filesystem[F])
        )
      }

    config.paths
      .traverse(determineFS)
      .flatMap { case Vector(source, _, target) =>
        implicit val filesystem: Filesystem[F] =
          new LoggingFilesystem(new VirtualizedFilesystem[F](
            Map(
              ("-source", source),
              ("-target", target),
              ("-os",     (Path.root, localFS))
            ),
            localFS
          )(??, Equal.equalRef))

        for {
          _ <- Vector("-target").traverse_(s => ensureDir(Path(s)))
          typer = new SimpleModTypes
          mods <- typer.allIn(Path("-source"))
          orderedMods <- mods.traverse { case (path, mod) =>
            initMod[F](mod).map(Keyed(toKey(path), _))
          }
          modMap = mods.map { case t @ (path, _) => toKey(path) -> t }.toMap
          labels = modMap.map { case (key, (_, mod)) => key -> mod.label }
          repoVar = new MutableVar(RepoState(orderedMods, labels))
        } yield new LocalMods(
          Path("-source"),
          repoVar,
          new MutableVar(modMap),
          typer,
          toKey,
          new MangleNames(Path("-target"))
        )
      }
      .widen
  }
}
