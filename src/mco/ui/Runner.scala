package mco.ui

import better.files._
import mco.data.{Key, Path}
import mco.io.generic.PrototypeImplementation
import mco.util.Capture
import monix.eval.{Coeval, Task, TaskApp}
import monix.scalaz._
import Capture.coeval._
import mco.core.{Deltas, Mods}
import mco.core.state.{ModState, RepoState}

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
      new Trigger.Effectful[Coeval](f => f(), ObjectProperty(UiState(None)))
    }

    val trigger = exec.run.fold ({
      case NonFatal(ex) =>
        implicit val dummyAlgebra: Mods[Coeval] = new Mods[Coeval] {
          override def update(key: Key, diff: Deltas.OfMod) = Coeval(())
          override def remove(key: Key) = Coeval(())
          override def liftFile(p: Path) = Coeval(None)
          override def state = Coeval(RepoState(Vector()))
        }
        new Trigger.Effectful[Coeval](_ => (), ObjectProperty(UiState(Some(ex.getMessage))))
    }, x => x)

    new MainWindow(trigger.state, trigger).main(args)
  }
}
