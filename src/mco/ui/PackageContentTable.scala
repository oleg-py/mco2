package mco.ui

import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.{TableColumn, TableView}
import scalafx.scene.layout.Priority

import mco.core.state.ContentState
import mco.ui.props._

class PackageContentTable(
  state: Prop[Vector[(String, ContentState)]]
) extends TableView[(String, ContentState)]{
  type Val = (String, ContentState)
  columnResizePolicy = TableView.ConstrainedResizePolicy
  editable = true
  vgrow = Priority.Sometimes
  items <== state
  columns ++= Seq(
    new NameColumn,
    new ContentKindColumn
  )

  class NameColumn extends TableColumn[Val, String] {
    maxWidth = Double.MaxValue
    text = "Name"
    cellValueFactory = c => ObjectProperty(c.value._1)
  }

  class ContentKindColumn extends TableColumn[Val, ContentState] {
    minWidth = 75
    text = "Kind"
    cellValueFactory = c => ObjectProperty(c.value._2)
    /*    cellFactory = _ => new ChoiceBoxTableCell[Content, ContentKind] {
          items ++= Seq(ContentKind.Mod, ContentKind.Doc, ContentKind.Garbage)
          editable = true
          converter = StringConverter(
            ContentKind.fromString _ andThen (_.orNull),
            ContentKind.asString
          )
        }
    onEditCommit = (ev: CellEditEvent[Content, ContentKind]) => {
      act(UpdateContentKind(ev.rowValue.key, ev.newValue))
    }*/
  }
}
