package mco.io.generic

import scalaz._
import std.vector._

import mco.core.Mods
import mco.core.state.{ModState, RepoState}
import mco.data.{Key, Keyed, Path}
import mco.io.state.{MmapHashing, MutableVar, SerializedVar, initMod}
import mco.util.Capture
import mco.util.syntax.fp._
import TempOps._

//noinspection ConvertibleToMethodValue
object PrototypeImplementation {
  def algebra[F[_]: Capture: Monad](root: Path): F[Mods[F]] = {
    val serialized = "mco2.dat"
    val repoDir = "mods"
    val target = "testTarget"
    implicit val hashing = new MmapHashing[F]
    implicit val tempOps = new LocalTempOps[F]
    implicit val filesystem = tempOps.filesystemF
    val typer = new SimpleModTypes

    val localModsF = for {
      mods <- typer.allIn(root / repoDir)
      modMap = mods.map { case t @ (path, _) => Key(path.asString) -> t }.toMap
      getState = mods.traverse { case (path, mod) =>
        runTmp[F, Keyed[ModState]] { f =>
          initMod[F](mod)
            .apply(f)
            .map(Keyed(Key(path.asString), _))
        }
      }
      repoVar <- SerializedVar[F, RepoState](
        root / serialized,
        getState.map(RepoState(_)),
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
