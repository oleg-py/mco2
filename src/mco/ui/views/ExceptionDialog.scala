package mco.ui.views

import scalafx.Includes._
import scalafx.scene.control._

import mco.ui.props._
import mco.ui.state.Commands

import java.io.{PrintWriter, StringWriter}

//noinspection ConvertibleToMethodValue
object ExceptionDialog {
  def apply(s: Prop[Option[Throwable]])(implicit dispatch: Commands) = {
    def show() = s().foreach(display(dispatch.closeErrorDialog _))
    show()
    s.onChange { show() }
  }

  def display(onClose: () => Unit)(err: Throwable) =
    new Alert(Alert.AlertType.Error) {
      title = "Error"
      headerText = "Could not complete operation"
      contentText = err.getMessage
      dialogPane().expandableContent = new TextArea {
        text = {
          val sw = new StringWriter()
          val pw = new PrintWriter(sw)
          err.printStackTrace(pw)
          pw.close()
          sw.toString
        }
        editable = false
      }
      onCloseRequest = handle { onClose() }
    }.showAndWait()
}
