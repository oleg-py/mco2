package mco.game.sims3

import cats.Functor
import fs2.Stream.eval
import mco.core.paths.Path
import mco.io.Filesystem


class Sims3PackContent(val path: Path) {
  def unpack[F[_]: Filesystem: Functor](s3ce: S3CE[F]): fs2.Stream[F, Path] =
    for {
      tmp    <- Filesystem.mkTemp
      target =  tmp / path.name
      _      <- eval(Filesystem.copy(path, target))
      _      <- eval(s3ce(target))
      outFile = target.withExtension(".package")
      outDir  = target.withExtension("")
      isFile <- eval(Filesystem.exists(outFile))
      path   <- if (isFile) fs2.Stream.emit(outFile).covary[F]
                else eval(Filesystem.childrenOf(outDir)).flatMap(fs2.Stream.emits(_))
    } yield path
}
