package mco.io.generic

import com.olegpy.forwarders
import mco.data.Path


@forwarders trait TempOps[F[_]] {
  val filesystemF: Filesystem[F]
  def runTmp[A](f: F[Path] => F[A]): F[A]
}
