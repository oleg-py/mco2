package mco.io

import com.olegpy.forwarders
import mco.core.paths.Path

// TODO - hashing might belong to this TC
@forwarders trait FileStamping[F[_]] {
  def likelySame(known: Path, file: Path): F[Boolean]
  def update(file: Path): F[Unit]
}
