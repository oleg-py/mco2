package mco.util

import java.io.{ByteArrayInputStream, InputStream}
import java.net.{URL, URLConnection, URLStreamHandler}
import cats._
import cats.syntax.applicativeError._

import mco.core.Capture

import java.util.Base64
import javax.swing.ImageIcon


object misc {
  def macOSIcon[F[_]: Capture: MonadError[?[_], Throwable]]: F[Unit] = Capture {
    val cls = Class.forName("com.apple.eawt.Application")
    cls.getMethod("setDockIconImage", classOf[java.awt.Image]).invoke(
      cls.getMethod("getApplication").invoke(null),
      new ImageIcon(getClass.getResource("/app-icon.png")).getImage
    )
    ()
  }.handleErrorWith(_ => Capture { () })

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
