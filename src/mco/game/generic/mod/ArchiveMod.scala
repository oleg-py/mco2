package mco.game.generic.mod

import scalaz._
import scalaz.std.vector._

import mco.core.Mod
import mco.core.paths._
import mco.io.{Archiving, InTemp}
import mco.util.syntax.fp._


class ArchiveMod[F[_]: Archiving: Functor](
  override val backingFile: Path
) extends Mod[F]
{
  override def list: F[Vector[RelPath]] =
    Archiving.entries(backingFile)

  override def provide(paths: Vector[RelPath]): InTemp[F, Map[RelPath, Path]] =
    InTemp.WithTemp { tempDir =>
      val targets = paths.fproduct(tempDir / _).toMap
      Archiving.extract(backingFile, targets) as targets
    }
}
