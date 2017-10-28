package mco.io.archives

import com.olegpy.forwarders
import mco.data.Path


@forwarders trait Archiving[F[_]] {
  def entriesOf(path: Path): F[Set[String]]
  def bulkExtract(path: Path, ft: Map[String, Path]): F[Unit]
}
