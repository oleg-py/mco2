package mco.ui.views

import cats.syntax.functor._
import scalafx.Includes._
import scalafx.geometry.{Insets, Orientation, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ToolBar}
import scalafx.scene.layout._
import scalafx.scene.paint.Paint
import scalafx.stage._

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
) extends StackPane with DropFilesReceiver { group =>
  margin = Insets(4, 0, 4, 0)
  alignment = Pos.TopRight

  children ++= Seq(
    ThumbnailImage,
    ThumbnailTools
  )

  def isImage(path: String) = isImg().apply(path)

  override def canAcceptFiles(paths: Vector[String]): Boolean = {
    paths match {
      case Vector(path) if isImage(path) => true
      case _ => false
    }
  }

  override def acceptFiles(paths: Vector[String]): Unit = {
    paths match {
      case Vector(path) if isImage(path) => cmd.setThumbnail(path)
      case _ => cmd.showNotification("Please drop a single file to set a thumbnail")
    }
  }

  object ThumbnailImage extends ImageViewPane {
    image <== state.map { urlOpt =>
      val url = urlOpt.getOrElse(getClass.getResource("/no-thumbnail.png"))
      new Image(url.toString)
    }
    smooth = true
    preserveRatio = true
  }

  class ImagePopup extends Stage { popup =>
    initModality(Modality.ApplicationModal)
    initOwner(group.getScene.getWindow)
    val img = state().map(_.toString).map(new Image(_)).orNull
    val pane = new ImageViewPane {
      image = img
      onMouseClicked = handle {
        popup.close()
      }
    }

    scene = new Scene {
      root = pane
    }
  }

  onMouseClicked = handle {
    state().foreach { _ =>
      new ImagePopup().showAndWait()
    }
  }

  class OpenButton extends Button("Open") {
    onAction = handle {
      val file = new FileChooser().showOpenDialog(scene().getWindow)
      val check = isImg()
      if ((file ne null) && check(file.toString)) {
        cmd.setThumbnail(file.toString)
      }
    }
  }

  object ClearButton extends Button("Clear") {
    disable <== state.map(_.isEmpty)
    onAction = handle {
      cmd.unsetThumbnail()
    }
  }

  object ThumbnailTools extends ToolBar {
    // Avoids duplicate children on lists sync
    val OpenButton1, OpenButton2 = new OpenButton
    items <== state map {
      case Some(_) => Seq(ClearButton, OpenButton1)
      case None => Seq(OpenButton2)
    }

    maxWidth = Region.USE_PREF_SIZE

    visible <== group.hover
    background = new Background(Array(new BackgroundFill(
      Paint.valueOf("rgba(0, 0, 0, 0.5)"),
      new CornerRadii(4),
      Insets(0)
    )))
  }
}
