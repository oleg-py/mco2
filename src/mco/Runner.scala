package mco

import scala.util.control.NonFatal
import scalafx.beans.property.ObjectProperty

import better.files._
import mco.core.ModStore
import mco.core.paths.Path
import mco.core.vars.MutableVar
import mco.game.generic.{StoreConfig, implementation}
import mco.ui.props.PropertyBasedVar
import mco.ui.state.{Commands, UiState}
import mco.ui.views.MainWindow
import monix.eval.{Coeval, Task, TaskApp}
import pureconfig.loadConfig

import java.nio.file.Paths


object Runner extends TaskApp {
  def readCwd = Coeval { Path(File(".").pathAsString) }

  def readConfig = Coeval {
    loadConfig[StoreConfig](Paths.get("./application.conf")).fold(
      fails => Coeval.raiseError(new Exception(fails.toList.mkString("\n"))),
      parsed => Coeval.now(parsed)
    )
  }.flatten

  override def run(args: Array[String]): Task[Unit] = Task.defer {
    val exec = for {
      _ <- JvmAddons.all[Coeval]
      config <- readConfig
      cwd <- readCwd
      algebras <- implementation.algebras[Coeval](config, cwd)
      repoMap = new ModStore.ByVar[Coeval](algebras, new MutableVar(0))
      states <- repoMap.states
      uiState = UiState.initial(repoMap.labels zip states, config.files.isImageS)
      mkCommands = Commands[Coeval](repoMap, coeval => coeval()) _
    } yield (uiState, mkCommands)

    exec
      .onErrorRecover { case NonFatal(ex) =>
        ex.printStackTrace()
        val repoMap = new ModStore.Empty[Coeval]
        val state = UiState.startupError(ex)
        val mkCommands = Commands[Coeval](repoMap, _ => ()) _
        (state, mkCommands)
      }
      .flatMap { case (initialState, mkDispatch) =>
        Coeval {
          val state = ObjectProperty(initialState)
          val dispatch = mkDispatch(new PropertyBasedVar(state))
          new MainWindow(state)(dispatch).main(args)
        }
      }
      .task
  }
}
