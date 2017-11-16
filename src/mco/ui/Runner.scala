package mco.ui

import better.files._
import mco.data.{Key, Path}
import mco.io.generic.{DummyMods, PrototypeImplementation}
import mco.util.Capture
import monix.eval.{Coeval, Task, TaskApp}
import monix.scalaz._
import Capture.coeval._
import mco.core.Mods
import scala.util.control.NonFatal
import scalafx.beans.property.ObjectProperty

object Runner extends TaskApp {
  override def run(args: Array[String]) = Task.eval {
    val cwd = Path(File(".").pathAsString)
    val exec = for {
      algebra <- PrototypeImplementation.algebra(cwd)
      state <- algebra.state
    } yield {
      implicit val mods: Mods[Coeval] = algebra
      new Dispatch.Effectful[Coeval](f => f(), ObjectProperty(UiState.initial(state)))
    }

    val dispatch = exec.run.fold ({
      case NonFatal(ex) =>
        ex.printStackTrace()
        implicit val dummyAlgebra = new DummyMods[Coeval]
        new Dispatch.Effectful[Coeval](_ => (), ObjectProperty(UiState.startupError(ex)))
    }, x => x)

    new MainWindow(dispatch.state)(dispatch).main(args)
  }
}
