package mco.game.generic.mod

import scalaz._
import scalaz.std.vector._

import mco.core.Mod
import mco.core.paths._
import mco.io.{Archiving, InTemp}
import mco.util.syntax.fp._


class ArchiveMod[F[_]: Archiving: Functor](archive: Path) extends Mod[F] {
  override val label: String = archive.name.toString
  override def list: F[Vector[RelPath]] =
    Archiving.entries(archive)

  override def provide(paths: Vector[RelPath]): InTemp[F, Map[RelPath, Path]] =
    InTemp.WithTemp { tempDir =>
      val targets = paths.fproduct(tempDir / _).toMap
      Archiving.extract(archive, targets) as targets
    }
}
