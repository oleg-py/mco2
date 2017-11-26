package mco.ui

import better.files._
import monix.eval.{Coeval, Task, TaskApp}
import monix.scalaz._
import mco.core.{Capture, ImageStore, Mods}
import Capture.coeval._
import scala.util.control.NonFatal
import scalafx.beans.property.ObjectProperty

import mco.core.vars.PrintingVar
import mco.data.paths.Path
import mco.stubs.{NoImageStore, NoMods}
import mco.ui.props.PropertyBasedVar
import mco.util.misc.{base64url, macOSIcon}
import mco.variant.generic._
import pureconfig.loadConfig


object Runner extends TaskApp {
  override def run(args: Array[String]): Task[Unit] = Task.defer {
    val cwd = Path(File(".").pathAsString)
    val configCoeval = loadConfig[GenericConfig]
      .fold(
        fails => Coeval.raiseError(new Exception(fails.toList.mkString("\n"))),
        parsed => Coeval.now(parsed))
    val exec = for {
      _ <- macOSIcon
      _ <- base64url
      config <- configCoeval
      algebras <- PrototypeImplementation.algebras(config, cwd)
      state <- algebras._1.state
    } yield {
      implicit val mods: Mods[Coeval] = algebras._1
      implicit val images: ImageStore[Coeval] = algebras._2
      val initialState = UiState.initial(state, config.files.isImageS)
      val mkCommands = Commands[Coeval](coeval => coeval()) _
      (initialState, mkCommands)
    }

    val recovered = exec.onErrorRecover { case NonFatal(ex) =>
      ex.printStackTrace()
      implicit val mods: Mods[Coeval] = new NoMods[Coeval]
      implicit val images: ImageStore[Coeval] = new NoImageStore[Coeval]
      val state = UiState.startupError(ex)
      val mkCommands = Commands[Coeval](_ => ()) _
      (state, mkCommands)
    }

    recovered
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
