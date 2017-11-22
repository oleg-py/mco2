package mco.util

import scalaz._
import std.anyVal._
import std.tuple._

import mco.data.Path
import mco.io.generic.Filesystem
import net.openhft.hashing.LongHashFunction
import mco.util.syntax.fp._
import java.util.UUID
import javax.swing.ImageIcon

import monix.eval.Coeval


object misc {
  val uuidName = { new UUID(_: Long, _: Long).toString }.tupled

  val strHashes = (str: String) => {
    val hi = LongHashFunction.xx(0L).hashChars(str)
    val lo = LongHashFunction.farmNa(0L).hashChars(str)
    (hi, lo)
  }

  def nextFreeName[F[_]: Filesystem: Monad](
    target: Path,
    ext: String,
    step: (Long, Long) = (1L, 3L))(
    hash: (Long, Long)
  ): F[Path] = {
    val candidate = target / (uuidName(hash) ++ ext)
    Filesystem.exists(candidate).ifM(
      nextFreeName(target, ext, step)(hash |+| step),
      candidate.point[F]
    )
  }

  val macOSIcon: Coeval[Unit] = Coeval {
    val cls = Class.forName("com.apple.eawt.Application")
    cls.getMethod("setDockIconImage", classOf[java.awt.Image]).invoke(
      cls.getMethod("getApplication").invoke(null),
      new ImageIcon(getClass.getResource("/app-icon.png")).getImage
    )
    ()
  }.onErrorHandle(_ => ())
}
