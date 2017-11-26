package mco.io.generic

import scalaz._
import std.vector._
import mco.util.syntax.fp._

import mco.core.{Content, Mod}
import mco.data.{Keyed, TempOp}
import mco.data.paths.{Path, RelPath}


class ArchiveMod[F[_]: Archiving: Monad](archive: Path) extends Mod[F] {
  override val label: String = archive.name.toString
  override def list: F[Vector[Keyed[Content]]] =
    for {
      entries <- Archiving.entries(archive)
    } yield entries.map(Content.Component(_))

  override def provide(paths: Vector[RelPath]): TempOp[F, Map[RelPath, Path]] =
    TempOp.WithTemp { tempDir =>
      val targets = paths.fproduct(tempDir / _).toMap
      Archiving.extract(archive, targets) as targets
    }
}
