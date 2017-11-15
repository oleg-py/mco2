package mco.ui

import scala.language.higherKinds
import scalafx.Includes._
import scalafx.scene.control._
import mco.ui.props._
import java.io.{PrintWriter, StringWriter}

object ExceptionDialog {
  def apply(s: Prop[Option[Throwable]], trigger: Trigger) = {
    def show() = s().foreach(display(trigger.closeErrorDialog _))
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
