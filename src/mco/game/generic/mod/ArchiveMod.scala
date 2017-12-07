package mco.game.generic.mod

import cats._
import cats.syntax.functor._
import cats.instances.vector._

import mco.core.Mod
import mco.core.paths._
import mco.io.{Archiving, InTemp}


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
