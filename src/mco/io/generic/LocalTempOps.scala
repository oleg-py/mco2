package mco.io.generic

import mco.data.Path
import mco.util.Capture
import better.files._

class LocalTempOps[F[_]: Capture] extends TempOps[F] {
  override val filesystemF = new LocalFilesystem[F]
  override def runTmp[A](f: F[Path] => F[A]) = {
    val file = Capture {
      Path(File.newTemporaryDirectory().pathAsString)
    }
    f(file) // TODO: resource cleanup is required there
  }
}
