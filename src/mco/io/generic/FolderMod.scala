package mco.io.generic

import scalaz._
import std.stream._
import syntax.id._
import syntax.std.boolean._

import mco.core._
import mco.util.syntax.fp._
import mco.util.syntax.any._
import Filesystem._
import mco.data._
import mco.data.paths.{Path, RelPath}
import mco.util.instances.mapRightMonoid

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

  override def provide(content: Vector[RelPath]): TempOp[F, Map[RelPath, Path]] = TempOp {
    for {
      data <- structureF
    } yield content
      .collect(data)
      .map(_.swap)
      .toMap
  }
}
