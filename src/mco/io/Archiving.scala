package mco.io

import com.olegpy.forwarders
import mco.core.paths.Path


@forwarders trait Archiving[F[_]] {
  def asArchive(path: Path): F[Archive[F]]
}
