package mco.io.generic

import com.olegpy.forwarders
import mco.data.Path


@forwarders trait TempFolder[F[_]] {
  def runTmp[A](f: F[Path] => F[A]): F[A]
}
