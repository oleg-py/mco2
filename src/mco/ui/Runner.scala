package mco.ui

import better.files._
import mco.data.Path
import mco.io.generic.PrototypeImplementation
import mco.util.Capture
import monix.eval.{Coeval, Task, TaskApp}
import monix.scalaz._
import Capture.coeval._
import mco.core.Mods
import scala.util.control.NonFatal
import scalafx.beans.property.ObjectProperty

import mco.stubs.NoMods
import mco.ui.props.PropertyBasedVar


object Runner extends TaskApp {
  override def run(args: Array[String]): Task[Unit] = Task.defer {
    val cwd = Path(File(".").pathAsString)
    val exec = for {
      algebra <- PrototypeImplementation.algebra(cwd)
      state <- algebra.state
    } yield {
      implicit val mods: Mods[Coeval] = algebra
      val initialState = UiState.initial(state)
      val mkDispatch = new Dispatch.Effectful[Coeval](coeval => coeval())(_)
      (initialState, mkDispatch)
    }

    val recovered = exec.onErrorRecover { case NonFatal(ex) =>
      ex.printStackTrace()
      implicit val dummyAlgebra: Mods[Coeval] = new NoMods[Coeval]
      val state = UiState.startupError(ex)
      val mkDispatch = new Dispatch.Effectful[Coeval](_ => ())(_)
      (state, mkDispatch)
    }

    recovered
      .flatMap { case (initialState, mkDispatch) =>
        Coeval {
          val state = ObjectProperty(initialState)
          val dispatch = mkDispatch(new PropertyBasedVar[UiState](state))
          new MainWindow(state)(dispatch).main(args)
        }
      }
      .task
  }
}
