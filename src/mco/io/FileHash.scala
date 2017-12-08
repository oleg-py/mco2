package mco.io

import cats.Functor
import cats.effect.Sync
import cats.instances.all._
import mco.core.paths.Path
import net.openhft.hashing.LongHashFunction

class FileHash[F[_]: Filesystem: Functor](file: Path) {
  def computed(implicit F: Sync[F]): F[(Long, Long)] =
    hashAt(file).runFoldMonoidSync

  private def hashFile(path: Path) =
    Filesystem.readFile(path).map { buffer =>
      val hi = LongHashFunction.xx(0L).hashBytes(buffer)
      val lo = LongHashFunction.farmNa(0L).hashBytes(buffer)
      (hi, lo)
    }

  private def hashDir(path: Path) =
    fs2.Stream.eval(Filesystem.childrenOf(path))
      .flatMap(paths => fs2.Stream(paths: _*).covary)
      .flatMap(hashAt)

  private def hashAt(path: Path): fs2.Stream[F, (Long, Long)] =
    fs2.Stream.eval(Filesystem.isDirectory(path))
      .flatMap(if (_) hashDir(path) else hashFile(path))
}
