package mco.io.generic

import com.olegpy.forwarders
import mco.data.paths.{Path, RelPath}


@forwarders trait Archiving[F[_]] {
  def entries(archive: Path): F[Vector[RelPath]]
  def extract(archive: Path, targets: Map[RelPath, Path]): F[Unit]
}
