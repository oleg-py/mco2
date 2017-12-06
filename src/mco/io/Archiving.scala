package mco.io

import com.olegpy.forwarders
import mco.core.paths._


@forwarders trait Archiving[F[_]] {
  def entries(archive: Path): F[Vector[RelPath]]
  def extract(archive: Path, targets: Map[RelPath, Path]): F[Unit]
}
