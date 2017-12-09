package mco.ui.views

import cats.syntax.functor._
import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.geometry.Insets
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.cell.{CheckBoxTableCell, TextFieldTableCell}
import scalafx.scene.control.{TableColumn, TableRow, TableView}
import scalafx.scene.layout.Priority

import mco.core.paths._
import mco.core.state.{ModState, RepoState}
import mco.ui.components.DropFilesReceiver
import mco.ui.props._
import mco.ui.state.Commands
import mco.syntax._

class RepoPackagesTable(state: Prop[RepoState])(implicit cmd: Commands)
  extends TableView[(RelPath, String, ModState)]
    with DropFilesReceiver
{ table =>
  type Triple = (RelPath, String, ModState)

  columns ++= Seq(CheckBoxColumn, TitleColumn)

  items <== state.map { case RepoState(mods, label) =>
    mods.map { case Pointed(k, v) => (k, label(k), v) }
  }

  editable = true
  hgrow = Priority.Always
  columnResizePolicy = TableView.ConstrainedResizePolicy
  rowFactory = _ => new PackageRow

  override def canAcceptFiles(path: Vector[String]): Boolean = true

  override def acceptFiles(paths: Vector[String]): Unit =
    cmd.addPending(paths)

  class PackageRow extends TableRow[Triple] {
    onMouseClicked = handle {
      if (!empty()) cmd.setActivePackage(item()._1)
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
    editable = true
    maxWidth <== table.width - 32 - 20 // 20 for horizontal scrollbar
    cellFactory = _ => new LabelEditTableCell
    cellValueFactory = s => ObjectProperty(s.value._2)
    onEditCommit = (ev: CellEditEvent[Triple, String]) => {
      cmd.setLabel(ev.rowValue._1, ev.newValue)
    }
  }

  private def mkCheckBox(i: Int) = {
    def tuple = table.items().get(i)
    BooleanProperty(tuple._3.stamp.installed)
      .tap(prop => prop.onChange {
        if (prop()) cmd.install(tuple._1)
        else cmd.uninstall(tuple._1)
      })
  }

  class InstallCheckbox extends CheckBoxTableCell[Triple, Any](mkCheckBox _) {
    padding = Insets(0)
  }

  class LabelEditTableCell extends TextFieldTableCell[Triple, String]() {
    editable = true
  }
}
