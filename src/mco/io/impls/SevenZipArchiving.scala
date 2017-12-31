package mco.io.impls

import cats.effect.Sync
import mco.core.paths.Path
import mco.io.{Archive, Archiving, Filesystem}
import mco.syntax._

class SevenZipArchiving[F[_]: Sync: Filesystem] extends Archiving[F] {
  override def asArchive(path: Path): F[Archive[F]] = capture {
    new SevenZipArchive[F](path)
  }
}
