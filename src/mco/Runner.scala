package mco

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
import monix.execution.misc.NonFatal
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
      state = ObjectProperty(uiState)
    } yield {
      lazy val commands: Commands = Commands[Coeval](repoMap, coeval => {
        coeval.onErrorHandle { case NonFatal(ex) =>
          commands.showError(ex)
        }.apply()
      }, new PropertyBasedVar(state))
      (state, commands)
    }

    exec
      .onErrorRecover { case NonFatal(ex) =>
        ex.printStackTrace()
        val repoMap = new ModStore.Empty[Coeval]
        val state = ObjectProperty(UiState.startupError(ex))
        val commands = Commands[Coeval](repoMap, _ => System.exit(0),
          new PropertyBasedVar(state))
        (state, commands)
      }
      .flatMap { case (state, commands) =>
        Coeval {
          new MainWindow(state)(commands).main(args)
        }
      }
      .task
  }
}
