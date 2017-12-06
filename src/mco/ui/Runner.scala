package mco.ui

import better.files._
import monix.eval.{Coeval, Task, TaskApp}
import monix.scalaz._
import mco.core.{Capture, RepoMap}
import Capture.coeval._
import scala.util.control.NonFatal
import scalafx.beans.property.ObjectProperty

import mco.core.paths.Path
import mco.core.vars.{MutableVar, PrintingVar}
import mco.game.generic.{GenericConfig, PrototypeImplementation}
import mco.ui.props.PropertyBasedVar
import mco.ui.state.{Commands, UiState}
import mco.ui.views.MainWindow
import mco.util.misc.{base64url, macOSIcon}
import pureconfig.loadConfig


object Runner extends TaskApp {
  def readCwd = Coeval { Path(File(".").pathAsString) }

  def initAddons = Coeval.sequence(Seq(macOSIcon, base64url))

  def readConfig = Coeval {
    loadConfig[GenericConfig].fold(
      fails => Coeval.raiseError(new Exception(fails.toList.mkString("\n"))),
      parsed => Coeval.now(parsed)
    )
  }.flatten

  override def run(args: Array[String]): Task[Unit] = Task.defer {
    val exec = for {
      _ <- initAddons
      config <- readConfig
      cwd <- readCwd
      algebras <- PrototypeImplementation.algebras(config, cwd)
      repoMap = new RepoMap.ByVar[Coeval](algebras, new MutableVar(0))
      states <- repoMap.states
      uiState = UiState.initial(repoMap.labels zip states, config.files.isImageS)
      mkCommands = Commands[Coeval](repoMap, coeval => coeval()) _
    } yield (uiState, mkCommands)

    exec
      .onErrorRecover { case NonFatal(ex) =>
        ex.printStackTrace()
        val repoMap = new RepoMap.Empty[Coeval]
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
