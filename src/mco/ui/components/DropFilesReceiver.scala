package mco.ui.components

import scalafx.Includes._
import scalafx.scene.Node
import scalafx.scene.input.{DragEvent, TransferMode}


trait DropFilesReceiver { this: Node =>
  def canAcceptFiles(path: Vector[String]): Boolean
  def acceptFiles(paths: Vector[String]): Unit

  private def filesIn(ev: DragEvent) =
    ev.dragboard.files.map(_.getAbsolutePath).toVector

  onDragOver = (ev: DragEvent) => {
    if (ev.dragboard.hasFiles && canAcceptFiles(filesIn(ev))) {
      ev.acceptTransferModes(TransferMode.Copy)
    } else {
      ev.consume()
    }
  }

  onDragDropped = (ev: DragEvent) => {
    val hadFiles = ev.dragboard.hasFiles
    val files = filesIn(ev)
    val canAccept = hadFiles && canAcceptFiles(files)
    if (canAccept) {
      acceptFiles(files)
    }
    ev.setDropCompleted(canAccept)
    ev.consume()
  }
}
