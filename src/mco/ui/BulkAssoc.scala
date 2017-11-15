package mco.ui

import javafx.scene.image.Image
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.cell.ChoiceBoxTableCell
import scalafx.scene.control.{Button, TableColumn, TableView}
import scalafx.scene.control.TableColumn._
import scalafx.Includes._
import scalafx.scene.layout.{HBox, Priority, VBox}
import scalafx.util.StringConverter.toStringConverter
import scalaz.std.vector._
import scalaz.std.option._

import better.files.File
import mco.ui.props._
import mco.util.syntax.fp._

import javafx.scene.control.TableColumn.CellEditEvent

class BulkAssoc(state: Prop[UiState.PendingAdds])(implicit dispatch: Dispatch) extends VBox { contentRoot =>
  private val table = new TableComponent

  hgrow = Priority.Always
  children = Seq(
    new ImageViewPane {
      image <== table.selectionModel.asProp
        .flatMap(_.selectedItemProperty.asProp)
        .map(Option.apply)
        .map {
          _.
            flatMap { case (_, maybeImg) => maybeImg }.
            map { path => new Image(File(path).uri.toString) }.
            orNull
        }
      preserveRatio = true
      smooth = true
      minHeight <== contentRoot.height * 2 / 3
    },
    table,
    new HBox {
      alignment = Pos.CenterRight
      padding = Insets(10, 0, 0, 0)
      spacing = 10
      children = Seq(
        new Button("Cancel") {
          prefWidth = 125
          cancelButton = true
          onAction = handle { dispatch.cancelPendingAdds() }
        },
        new Button("Submit") {
          prefWidth = 125
          defaultButton = true
          onAction = handle { dispatch.applyPendingAdds() }
        }
      )
    }
  )

  private class TableComponent extends TableView[(String, Option[String])] { table =>
    editable = true
    columnResizePolicy = TableView.ConstrainedResizePolicy
    items <== state.map(add => add.packages fproduct add.assoc)

    columns ++= Seq(
      new TableColumn[(String, Option[String]), String] {
        text = "Package file"
        cellValueFactory = s => ObjectProperty(lastPathSegment(s.value._1))
      },

      new TableColumn[(String, Option[String]), Option[String]] {
        text = "Package thumbnail"
        editable = true
        cellValueFactory = s => ObjectProperty(s.value._2)
        cellFactory = _ =>
          new ChoiceBoxTableCell[(String, Option[String]), Option[String]](pathToString) {
            items <== state.map(none +: _.images.map(_.some))
          }

        onEditCommit = (ev: CellEditEvent[(String, Option[String]), Option[String]]) => {
          dispatch.associatePending(ev.rowValue._1, ev.newValue)
        }
      }
    )

    private def lastPathSegment(s: String) = {
      val m = Math.max(s.lastIndexOf('/'), s.lastIndexOf('\\'))
      if (m == -1) s else s.drop(m + 1)
    }

    private val pathToString = toStringConverter[Option[String]](_.fold("<none>")(lastPathSegment))
  }
}
