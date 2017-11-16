package mco.ui.components

import scalafx.Includes._
import scalafx.scene.Node
import scalafx.scene.input.{DragEvent, TransferMode}


trait DropFilesReceiver { this: Node =>
  def onFilesReceived(paths: Vector[String]): Unit

  onDragOver = (ev: DragEvent) => {
    if (ev.dragboard.hasFiles) {
      ev.acceptTransferModes(TransferMode.Copy)
    } else {
      ev.consume()
    }
  }

  onDragDropped = (ev: DragEvent) => {
    val hadFiles = ev.dragboard.hasFiles
    if (hadFiles) {
      onFilesReceived(ev.dragboard.files.map(_.getAbsolutePath).toVector)
    }
    ev.setDropCompleted(hadFiles)
    ev.consume()
  }
}
