package mco.io.impls

import scalaz.Monad

import mco.core.paths._
import mco.core.vars.Var
import mco.io.{FileStamping, Filesystem}
import mco.io.impls.FilesystemVarStamping.FileStamp
import mco.util.syntax.fp._

class FilesystemVarStamping[F[_]: Filesystem: Monad](
  data: Var[F, Map[InnerPath, FileStamp]]
)
  extends FileStamping[F]
{
  override def likelySame(known: InnerPath, actual: Path, file: Path): F[Boolean] = {
    val expected = data().map(_.get(known))
    val current  = noHashStamp(actual)
    val existing   = getStamp(file)

    val isStale = (expected |@| current) {
      case (None, _) => false
      case (Some(st), Some((size, time))) => st.size != size || st.time != time
      case (Some(_), None) => true
    }

    def compare = (expected |@| existing) { _ == _ }
    def invalidate = data ~= (_ - known)

    isStale.ifM(invalidate.as(false), compare)
  }

  override def overwrite(value: InnerPath, actual: Path): F[Unit] =
    for {
      stampOpt <- getStamp(actual)
      map      <- data()
      modified =  stampOpt.cata(map.updated(value, _), map - value)
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