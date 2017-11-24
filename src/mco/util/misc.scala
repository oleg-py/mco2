package mco.util

import java.io.{ByteArrayInputStream, InputStream}
import java.net.{URL, URLConnection, URLStreamHandler}
import scalaz._
import std.anyVal._
import std.tuple._

import mco.core.Capture
import mco.data.paths.Path
import mco.io.generic.Filesystem
import net.openhft.hashing.LongHashFunction
import mco.util.syntax.fp._

import java.util.{Base64, UUID}
import javax.swing.ImageIcon
import monix.eval.Coeval


object misc {
  val uuidName = { new UUID(_: Long, _: Long).toString }.tupled

  val strHashes = (str: String) => {
    val hi = LongHashFunction.xx(0L).hashChars(str)
    val lo = LongHashFunction.farmNa(0L).hashChars(str)
    (hi, lo)
  }

  def macOSIcon[F[_]: Capture: MonadError[?[_], Throwable]]: F[Unit] = Capture {
    val cls = Class.forName("com.apple.eawt.Application")
    cls.getMethod("setDockIconImage", classOf[java.awt.Image]).invoke(
      cls.getMethod("getApplication").invoke(null),
      new ImageIcon(getClass.getResource("/app-icon.png")).getImage
    )
    ()
  }.handleError(_ => Capture { () })

  private val dataUrl = "data:(.*?);(.*?),(.*)".r
  private class DataConnection(u: URL) extends URLConnection(u) {
    override def connect(): Unit = ()

    override def getInputStream: InputStream = {
      u.toString match {
        case dataUrl(_, "base64", content) =>
          new ByteArrayInputStream(Base64.getDecoder.decode(content))
        case dataUrl(_, scheme, _) =>
          throw new Exception(s"Unsupported data URI scheme: $scheme")
        case _ =>
          throw new Exception(s"Could not read $u")
      }
    }
  }

  def base64url[F[_]: Capture] = Capture {
    val handler: URLStreamHandler = new DataConnection(_)
    URL.setURLStreamHandlerFactory(protocol => if (protocol == "data") handler else null)
  }
}
