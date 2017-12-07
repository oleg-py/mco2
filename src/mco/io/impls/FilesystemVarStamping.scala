package mco.io.impls

import scalaz.Monad

import mco.core.paths.Path
import mco.core.vars.Var
import mco.io.{FileStamping, Filesystem}
import mco.io.impls.FilesystemVarStamping.FileStamp
import mco.util.syntax.fp._

class FilesystemVarStamping[F[_]: Filesystem: Monad](
  data: Var[F, Map[Path, FileStamp]]
)
  extends FileStamping[F]
{
  override def likelySame(known: Path, file: Path): F[Boolean] = {
    val expected = data().map(_.get(known))
    val current  = noHashStamp(known)
    val actual   = getStamp(file)

    val isStale = (expected |@| current) {
      case (None, _) => false
      case (Some(st), Some((size, time))) => st.size != size || st.time != time
      case (Some(_), None) => true
    }

    def compare = (expected |@| actual) { _ == _ }
    def invalidate = data ~= (_ - known)

    isStale.ifM(invalidate.as(false), compare)
  }

  override def update(file: Path): F[Unit] =
    for {
      stampOpt <- getStamp(file)
      map      <- data()
      modified =  stampOpt.cata(map.updated(file, _), map - file)
      _        <- data := modified
    } yield ()

  private def noHashStamp(file: Path): F[Option[(Long, Long)]] =
    for (statOpt <- Filesystem.stat(file)) yield statOpt.map(stat =>
      (stat.size(), stat.lastModifiedTime().toMillis)
    )

  private def getStamp(file: Path): F[Option[FileStamp]] =
    for {
      statOpt <- noHashStamp(file)
      hash    <- Filesystem.hashAt(file)
    } yield statOpt.map { case (size, time) =>
      FileStamp(size, time, hash._1, hash._2)
    }
}

object FilesystemVarStamping {
  private case class FileStamp(size: Long, time: Long, hashHi: Long, hashLo: Long)
}