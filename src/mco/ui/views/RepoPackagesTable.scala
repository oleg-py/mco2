package mco.ui.views

import cats.syntax.functor._
import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.geometry.Insets
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.cell.{CheckBoxTableCell, TextFieldTableCell}
import scalafx.scene.control._
import scalafx.scene.input.{ClipboardContent, DataFormat, TransferMode}
import scalafx.scene.layout._
import scalafx.scene.paint.Paint

import mco.core.Status
import mco.core.paths._
import mco.core.state.{ModState, RepoState}
import mco.ui.components.DropFilesReceiver
import mco.ui.props._
import mco.ui.state.Commands
import mco.syntax._

import javafx.scene.input.{DragEvent, MouseEvent}

class RepoPackagesTable(state: Prop[RepoState])(implicit cmd: Commands)
  extends TableView[(RelPath, String, ModState)]
    with DropFilesReceiver
{ table =>
  type Triple = (RelPath, String, ModState)

  columns ++= Seq(CheckBoxColumn, TitleColumn)

  items <== state.map { case RepoState(mods, label) =>
    mods.map { case (k, v) => (k, label(k), v) }
  }

  val item = selectionModel.value.selectedItemProperty().asProp

  item
    .onChange { (evt, _, _) =>
      // selected item might be null
      Option(evt()).map(_._1)
        .foreach(cmd.setActivePackage)
    }

  contextMenu = new ContextMenu(
    new MenuItem {
      text <== item.map { value =>
        Option(value)
          .map(_._3.status)
          .getOrElse(Status.Unused) match {
          case Status.Unused    => "Install"
          case Status.Installed => "Uninstall"
        }
      }

      onAction = handle {
        cmd.toggleActive()
      }
    },
    new MenuItem("Remove") {
      onAction = handle {
        cmd.removeActive()
      }
    }
  )

  editable = true
  hgrow = Priority.Always
  columnResizePolicy = TableView.ConstrainedResizePolicy
  rowFactory = _ => new PackageRow


  override def canAcceptFiles(path: Vector[String]): Boolean = true

  override def acceptFiles(paths: Vector[String]): Unit =
    cmd.addPending(paths)

  class PackageRow extends TableRow[Triple] { self =>
    onDragDetected = (ev: MouseEvent) => {
      if (!empty()) {
        val db = self.startDragAndDrop(TransferMode.Move)
        db.setDragView(self.snapshot(null, null))
        val cc = new ClipboardContent
        cc.put(RepoPackagesTable.intFormat, self.index(): Integer)
        db.setContent(cc)
        ev.consume()
      }
    }

    onDragOver = (event: DragEvent) => {
      val db = event.getDragboard
      if (db.hasContent(RepoPackagesTable.intFormat)) {
        if (self.index() != db.getContent(RepoPackagesTable.intFormat).asInstanceOf[Int]) {
          event.acceptTransferModes(TransferMode.Move)
          event.consume()
        }
      }
    }
  }

  object CheckBoxColumn extends TableColumn[Triple, Any] {
    text = "S."
    maxWidth = 32
    resizable = false
    editable = true
    cellFactory = _ => new InstallCheckbox
  }

  object TitleColumn extends TableColumn[Triple, String] {
    text = "Package name"
    editable = false
    maxWidth <== table.width - 32 - 20 // 20 for horizontal scrollbar
    cellFactory = _ => new LabelTableCell
    cellValueFactory = s => ObjectProperty(s.value._2)
  }

  private def mkCheckBox(i: Int) = {
    def tuple = table.items().get(i)
    val prop = BooleanProperty(tuple._3.status == Status.Installed)
    prop.onChange {
      if (prop()) cmd.install(tuple._1)
      else cmd.uninstall(tuple._1)
    }
    prop
  }

  class InstallCheckbox extends CheckBoxTableCell[Triple, Any](mkCheckBox _) {
    padding = Insets(0)
  }

  class LabelTableCell extends TextFieldTableCell[Triple, String]() {
    editable = false
  }
}

object RepoPackagesTable {
  private val intFormat = new DataFormat("application/x-integer")
}
