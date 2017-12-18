package mco.ui.views

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.control.TableColumn.CellEditEvent
import scalafx.scene.control.cell.ChoiceBoxTableCell
import scalafx.scene.control.{TableColumn, TableView}
import scalafx.util.StringConverter

import mco.core.ContentKind
import mco.core.paths.RelPath
import mco.core.state.ContentState
import mco.ui.props._
import mco.ui.state.Commands

class PackageContentTable(
  state: Prop[Vector[(RelPath, String, ContentState)]]
)(
  implicit cmd: Commands
) extends TableView[(RelPath, String, ContentState)]{
  type Val = (RelPath, String, ContentState)
  columnResizePolicy = TableView.ConstrainedResizePolicy
  editable = true
  items <== state
  columns ++= Seq(
    new NameColumn,
    new ContentKindColumn
  )
  margin = Insets(4, 0, 4, 0)

  class NameColumn extends TableColumn[Val, String] {
    maxWidth = Double.MaxValue
    text = "Name"
    cellValueFactory = c => ObjectProperty(c.value._2)
  }

  class ContentKindColumn extends TableColumn[Val, ContentKind] {
    minWidth = 85
    text = "Kind"
    cellValueFactory = c => ObjectProperty(c.value._3.assignedKind)
    cellFactory = _ => new ChoiceBoxTableCell[Val, ContentKind] {
      items ++= Seq(
        ContentKind.Component,
        ContentKind.Document,
        ContentKind.Unused
      )
      editable = true
      converter = StringConverter(
        ContentKind.withNameInsensitiveOption _ andThen (_.orNull),
        _.entryName
      )
    }
    onEditCommit = (ev: CellEditEvent[Val, ContentKind]) => {
      val (key, _, _) = ev.rowValue
      cmd.setCurrentKind(key, ev.newValue)
    }
  }
}
