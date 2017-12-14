package mco.stubs

import cats.Applicative
import cats.syntax.applicative._
import mco.core.paths.{InnerPath, Path, RelPath}
import mco.io.FileStamping


/**
 * FileStamping implementation that does not track any state
 */
class NoStamping[F[_]: Applicative] extends FileStamping[F] {
  override def likelySame(known: InnerPath, actual: Path, file: Path): F[Boolean] =
    false.pure[F]

  override def overwrite(value: InnerPath, actual: Path): F[Unit] = ().pure[F]
}
