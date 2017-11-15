package mco.ui


import scalafx.application
import scalafx.application.JFXApp
import scalafx.beans.property.ObjectProperty

class MainWindow(state: ObjectProperty[UiState], trigger: Trigger) extends JFXApp {
  stage = new application.JFXApp.PrimaryStage {
    title.value = state.value.error.getOrElse("No error!")
  }
}

