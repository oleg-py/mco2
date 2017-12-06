package mco.ui.views

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.TabPane
import scalaz.syntax.functor._

import mco.ui.props._
import mco.ui.state.{Commands, UiState}

import javafx.scene.image.Image


class MainWindow(state: Prop[UiState])(implicit cmd: Commands) extends JFXApp {
  Thread.currentThread.setUncaughtExceptionHandler((_, throwable) => {
    throwable.printStackTrace()
  })

  stage = new JFXApp.PrimaryStage {
    ExceptionDialog(state.asProp.map(_.error))

    width = 800
    height = 600
    title = "Mod Collection Organizer"
    scene = new Scene {
      stylesheets += "/mco.ui/main.css"
      root = new TabPane {
        // TODO
        tabs = state().tabs.indices.map { i =>
          val tabState = state.map(_.tabs(i))
          val isImage = state.map(_.isImage)
          new RepoTab(tabState, isImage)
        }

        selectionModel().selectedIndexProperty() onChange {
          val index = selectionModel().getSelectedIndex
          cmd.setActiveTab(index)
        }
      }
    }
  }

  stage.icons += new Image(getClass.getResourceAsStream("/app-icon.png"))
}

