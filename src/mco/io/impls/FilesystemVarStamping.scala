package mco.io.impls

import cats._
import cats.instances.option._
import cats.effect.Sync
import cats.syntax.all._
import mco.core.paths._
import mco.core.vars.Var
import mco.io.{FileHash, FileStamping, Filesystem}
import mco.io.impls.FilesystemVarStamping.FileStamp

class FilesystemVarStamping[F[_]: Filesystem: Sync](
  data: Var[F, Map[InnerPath, FileStamp]],
)
  extends FileStamping[F]
{
  override def likelySame(known: InnerPath, actual: Path, file: Path): F[Boolean] = {
    val expected = data().map(_.get(known))
    val current  = noHashStamp(actual)
    val existing = getStamp(file)

    val isStale = (expected, current).mapN {
      case (Some(st), Some((size, time))) => st.size != size || st.time != time
      case _ => true
    }

    def compare = (expected, existing).mapN(_ == _)
    def invalidate = data ~= (_ - known)

    isStale.ifM(invalidate.as(false), compare)
  }

  override def overwrite(value: InnerPath, actual: Path): F[Unit] =
    for {
      stampOpt <- getStamp(actual)
      map      <- data()
      modified =  stampOpt.fold(map - value)(map.updated(value, _))
      _        <- data := modified
    } yield ()

  private def noHashStamp(file: Path): F[Option[(Long, Long)]] =
    for (statOpt <- Filesystem.stat(file)) yield statOpt.map(stat =>
      (stat.size(), stat.lastModifiedTime().toMillis)
    )

  private def getStamp(file: Path): F[Option[FileStamp]] =
    noHashStamp(file) >>= (_.traverse { case (size, time) =>
      new FileHash(file).computed.map { case (hi, lo) =>
        FileStamp(size, time, hi, lo)
      }
    })
}

object FilesystemVarStamping {
  def defaultState = Map.empty[InnerPath, FileStamp]
  case class FileStamp(size: Long, time: Long, hashHi: Long, hashLo: Long)
}