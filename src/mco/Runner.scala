package mco

import scalafx.beans.property.ObjectProperty

import better.files._
import mco.core.RepoSeq
import mco.core.paths.Path
import mco.core.vars.MutableVar
import mco.game.generic.StoreConfig.Tools
import mco.game.generic.{StoreConfig, implementation}
import mco.ui.ActionQueue
import mco.ui.props.PropertyBasedVar
import mco.ui.state.{Commands, UiState}
import mco.ui.views.MainWindow
import monix.eval.{Coeval, Task, TaskApp}
import monix.execution.misc.NonFatal
import pureconfig._

import java.nio.file.Paths


object Runner extends TaskApp {
  def readCwd = Coeval { Path(File(".").pathAsString) }

  def readConfig = Coeval.defer {
    // uses name "s3ce" instead of "s-3ce"
    implicit val hint: ProductHint[Tools] =
      ProductHint[Tools](ConfigFieldMapping(CamelCase, CamelCase))
    loadConfig[StoreConfig](Paths.get("./application.conf")).fold(
      fails => Coeval.raiseError(new Exception(fails.toList.mkString("\n"))),
      parsed => Coeval.now(parsed)
    )
  }

  override def run(args: Array[String]): Task[Unit] = Task.deferAction { implicit sc =>
    val queue = new ActionQueue

    val exec = for {
      _        <- JvmAddons.all[Coeval]
      config   <- readConfig
      cwd      <- readCwd
      algebras <- implementation[Coeval](config, cwd)
      counter  <- MutableVar[Coeval, Int](0)
      repoMap  =  new RepoSeq.ByVar[Coeval](algebras, counter)
      states   <- repoMap.states
    } yield {
      val uiState = UiState.initial(repoMap.labels zip states, config.files.isImagePath)
      val state = ObjectProperty(uiState)
      val commands: Commands = Commands[Coeval](
        repoMap,
        queue.enqueue,
        new PropertyBasedVar(state))
      (state, commands)
    }

    exec
      .onErrorRecover { case NonFatal(ex) =>
        ex.printStackTrace()
        val repoMap = new RepoSeq.Empty[Coeval]
        val state = ObjectProperty(UiState.startupError(ex))
        val commands = Commands[Coeval](repoMap, _ => System.exit(0),
          new PropertyBasedVar(state))
        (state, commands)
      }
      .flatMap { case (state, commands) =>
        Coeval {
          queue.watch.foreach(commands.showError)
          new MainWindow(state)(commands).main(args)
        }
      }
      .task
  }
}
