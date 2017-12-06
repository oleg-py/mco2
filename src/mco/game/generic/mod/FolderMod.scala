package mco.game.generic.mod

import scalaz._
import std.stream._
import syntax.id._
import syntax.std.boolean._

import mco.core._
import mco.core.paths._
import mco.io.{Filesystem, InTemp}, Filesystem._
import mco.util.syntax.any._
import mco.util.syntax.fp._

class FolderMod[F[_]: Filesystem: Monad](self: Path)
  extends Mod[F]
{
  private type DataM = Map[RelPath, (Path, RelPath)]

  override val label: String = self.name.toString

  private def toState[A](fa: F[A]): StateT[F, DataM, A] =
    StateT(s => fa.strengthL(s))

  private def scanDeepRec(fa: F[Stream[Path]]) =
    toState(fa).flatMap(_ traverse_ scanDeep)

  private def scanDeep(path: Path): StateT[F, DataM, Unit] = {
    import mco.util.instances.monoidForApplicative
    val MS = MonadState[StateT[F, DataM, ?], DataM]

    for {
      isDir   <- isDirectory(path) |> toState
      _       <- scanDeepRec(isDir ?? childrenOf(path))
      key     =  path relTo self
      _       <- if (isDir) MS.point(unit)
                 else MS.modify(_.updated(key, (path, key)))
    } yield ()
  }

  /*_*/
  private val structureF: F[DataM] =
    scanDeepRec(childrenOf(self)).exec(Map())
  /*_*/

  override def list: F[Vector[RelPath]] = structureF.map { data =>
    data.map { case (_, (_, c)) => c } .toVector
  }

  override def provide(content: Vector[RelPath]): InTemp[F, Map[RelPath, Path]] = InTemp {
    for {
      data <- structureF
    } yield content
      .collect(data)
      .map(_.swap)
      .toMap
  }
}
