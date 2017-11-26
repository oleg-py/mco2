package mco.ui.views


import scalafx.geometry.Insets
import scalaz.syntax.functor._

import mco.ui.Commands
import mco.ui.components.{DropFilesReceiver, ImageViewPane}
import mco.ui.props._

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
  image <== state.map(_.map(url => new Image(url.toString)).orNull)
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
