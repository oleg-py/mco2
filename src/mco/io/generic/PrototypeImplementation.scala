package mco.io.generic

import scalaz._
import std.vector._

import mco.core.Mods
import mco.core.state.RepoState
import mco.core.vars._
import mco.data.{Key, Keyed, Path}
import mco.io.state.initMod
import mco.util.Capture
import mco.util.syntax.fp._
import Filesystem._
import mco.stubs.{Cell, LoggingFilesystem, VarFilesystem}

//noinspection ConvertibleToMethodValue
object PrototypeImplementation {
  def algebra[F[_]: Capture: Monad](root: Path): F[Mods[F]] = {
    val target = root / "testing"
    val toKey = (p: Path) => Key(p.name)

    val localFS = new LocalFilesystem[F]
    val var0 = SerializedVar[F, Cell.Dir](
      target / "store.dat",
      Cell.Dir(),
      new MutableVar(_).point[F].widen
    )(implicitly, localFS)

    val result = for (rootVar <- var0) yield {
      val varFS = new VarFilesystem[F](new PrintingVar(rootVar))
      implicit val fsEq: Equal[Filesystem[F]] = Equal.equalRef
      implicit val filesystem: Filesystem[F] =
        new LoggingFilesystem(new VirtualizedFilesystem[F](
          Map(
            ("-source", (Path.root, varFS)),
            ("-target", (target / "installed", localFS)),
            ("-os", (Path.root, localFS))
          ),
          localFS
        ))

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

    result.join.widen
  }
}
