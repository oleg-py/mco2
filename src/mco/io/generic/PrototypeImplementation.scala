package mco.io.generic

import scalaz._
import std.vector._
import mco.core.{Deltas, Mods}
import mco.core.state.{ModState, RepoState}
import mco.data.{Key, Keyed, Path}
import mco.io.state.{MutableVar, SerializedVar, initMod}
import mco.util.Capture
import mco.util.syntax.fp._
import Filesystem._

//noinspection ConvertibleToMethodValue
object PrototypeImplementation {
  def algebra[F[_]: Capture: Monad](root: Path): F[Mods[F]] = {
    val serialized = "mco2.dat"
    val repoDir = "mods"
    val target = "testTarget"
    implicit val filesystem = new LocalFilesystem[F]
    val typer = new SimpleModTypes

    val localModsF = for {
      _ <- ensureDir(root / repoDir)
      _ <- ensureDir(root / target)
      mods <- typer.allIn(root / repoDir)
      modMap = mods.map { case t @ (path, _) => Key(path.asString) -> t }.toMap
      getState = mods.traverse { case (path, mod) =>
        initMod[F](mod).map(Keyed(Key(path.asString), _))
      }
      labels = modMap.map { case (key, (_, mod)) => key -> mod.label }
      repoVar <- SerializedVar[F, RepoState](
        root / serialized,
        getState.map(RepoState(_, labels)),
        new MutableVar(_)
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
