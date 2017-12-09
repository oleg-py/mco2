package mco.io

import com.olegpy.forwarders
import mco.core.paths._

@forwarders trait FileStamping[F[_]] {
  def likelySame(known: InnerPath, actual: Path, file: Path): F[Boolean]
  def overwrite(value: InnerPath, actual: Path): F[Unit]
}
