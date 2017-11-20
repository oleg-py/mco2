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

//noinspection ConvertibleToMethodValue
object PrototypeImplementation {
  def algebra[F[_]: Capture: Monad](root: Path): F[Mods[F]] = {
    val serialized = "mco2.dat"
    val repoDir = "mods"
    val target = "testTarget"
    implicit val filesystem: Filesystem[F] = new LocalFilesystem[F]
    val typer = new SimpleModTypes

    val localModsF = for {
      _ <- ensureDir(root / repoDir)
      _ <- ensureDir(root / target)
      mods <- typer.allIn(root / repoDir)
      modMap = mods.map { case t @ (path, _) => Key(path.asString) -> t }.toMap
      oldState <- mods.traverse { case (path, mod) =>
        initMod[F](mod).map(Keyed(Key(path.asString), _))
      }
      labels = modMap.map { case (key, (_, mod)) => key -> mod.label }
      repoVar <- SerializedVar[F, RepoState](
        root / serialized,
        RepoState(oldState, labels),
        new MutableVar(_).point[F].widen
      )
    } yield new LocalMods(
      root / repoDir,
      repoVar,
      new MutableVar(modMap),
      typer,
      new MangleNames(root / target)
    )

    localModsF.widen
  }
}
