package mco.ui


import scalafx.application
import scalafx.application.JFXApp
import scalafx.beans.property.ObjectProperty
import scalafx.Includes._
import mco.ui.props._
import scalaz.syntax.functor._

class MainWindow(state: Prop[UiState], trigger: Trigger) extends JFXApp {
  stage = new application.JFXApp.PrimaryStage {
    ExceptionDialog(state.asProp.map(_.error), trigger)
  }
}

