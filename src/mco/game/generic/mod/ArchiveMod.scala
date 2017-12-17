package mco.game.generic.mod

import cats._
import cats.syntax.functor._
import cats.instances.vector._
import mco.core.Mod
import mco.core.paths._
import mco.io._


class ArchiveMod[F[_]: Filesystem: Functor](archive: Archive[F]) extends Mod[F] {
  override val backingFile: Path = archive.file

  override def list: F[Vector[RelPath]] = archive.entries

  override def provide(content: Vector[RelPath]): fs2.Stream[F, Pointed[Path]] =
    Filesystem.mkTemp
      .evalMap { tmpDir =>
        val targets = content.fproduct(tmpDir / _).toMap
        val result = content.map(rel => Pointed(rel, tmpDir / rel))
        archive.extract(targets).as(result)
      }
    .flatMap(fs2.Stream.emits(_))
}
