package mco.ui

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control.TabPane

import mco.ui.props._
import scalaz.syntax.functor._

import mco.ui.views.RepoTab

import javafx.scene.image.Image


class MainWindow(state: Prop[UiState])(implicit dispatch: Dispatch) extends JFXApp {
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
      root = new TabPane { tabs = Seq(new RepoTab(state)) }
    }
  }

  stage.icons += new Image(getClass.getResourceAsStream("/app-icon.png"))
}

