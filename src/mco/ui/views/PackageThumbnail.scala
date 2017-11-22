package mco.ui.views

import scalafx.scene.layout.Priority
import scalaz.syntax.functor._

import mco.ui.Dispatch
import mco.ui.components.{DropFilesReceiver, ImageViewPane}
import mco.ui.props._

import java.net.URL
import javafx.scene.image.Image

class PackageThumbnail(state: Prop[Option[URL]], parentHeight: Prop[Double])(
  implicit dispatch: Dispatch)
  extends ImageViewPane with DropFilesReceiver
{
  image <== state.map(_.map(url => new Image(url.toString)).orNull)
  smooth = true
  preserveRatio = true

  minHeight <== parentHeight
  hgrow = Priority.Always
  vgrow = Priority.Always

  override def onFilesReceived(paths: Vector[String]): Unit = {
    paths match {
      case Vector(path) => dispatch.setThumbnail(path)
      case _ => dispatch.showNotification("Please drop a single file to set a thumbnail")
    }
  }
}
