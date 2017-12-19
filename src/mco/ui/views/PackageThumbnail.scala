package mco.ui.views


import cats.syntax.functor._
import scalafx.geometry.Insets

import mco.ui.components.{DropFilesReceiver, ImageViewPane}
import mco.ui.props._
import mco.ui.state.Commands

import java.net.URL
import javafx.scene.image.Image

class PackageThumbnail(
  state: Prop[Option[URL]],
  isImg: Prop[String => Boolean]
)(implicit
  cmd: Commands
)
  extends ImageViewPane with DropFilesReceiver
{
  image <== state.map { urlOpt =>
    val url = urlOpt.getOrElse(getClass.getResource("/no-thumbnail.png"))
    new Image(url.toString)
  }
  smooth = true
  preserveRatio = true
  margin = Insets(4, 0, 4, 0)

  def isImage(path: String) = isImg().apply(path)

  override def canAcceptFiles(paths: Vector[String]): Boolean = {
    paths match {
      case Vector(path) if isImage(path) => true
      case _ => false
    }
  }

  override def acceptFiles(paths: Vector[String]): Unit = {
    paths match {
      case Vector(path) => cmd.setThumbnail(path)
      case _ => cmd.showNotification("Please drop a single file to set a thumbnail")
    }
  }
}
