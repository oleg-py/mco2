package mco.io.generic

import scalaz._
import scalaz.std.vector._

import mco.core.Mod
import mco.data.TempOp
import mco.data.paths.{Path, RelPath}
import mco.util.syntax.fp._


class ArchiveMod[F[_]: Archiving: Functor](archive: Path) extends Mod[F] {
  override val label: String = archive.name.toString
  override def list: F[Vector[RelPath]] =
    Archiving.entries(archive)

  override def provide(paths: Vector[RelPath]): TempOp[F, Map[RelPath, Path]] =
    TempOp.WithTemp { tempDir =>
      val targets = paths.fproduct(tempDir / _).toMap
      Archiving.extract(archive, targets) as targets
    }
}
