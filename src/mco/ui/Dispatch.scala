package mco.ui

import mco.core.{Deltas, Mods}
import mco.core.state.RepoState
import mco.data.{Key, Path}
import mco.util.Capture

import scalafx.beans.property.ObjectProperty
import scalaz.Monad
import scalaz.syntax.monad._

trait Dispatch {
  def setLabel(key: Key, label: String): Unit = ???

  def install(key: Key): Unit = ???
  def uninstall(key: Key): Unit = ???

  def setActivePackage(key: Key) = ???
  def liftPackages(paths: Vector[String]): Unit = ???

  def update(key: Key, diff: Deltas.OfMod): Unit
  def remove(key: Key): Unit
  def liftFile(p: Path): Unit
  def closeErrorDialog(): Unit

  def associatePending(k: String, v: Option[String]): Unit
  def applyPendingAdds(): Unit
  def cancelPendingAdds(): Unit
}

object Dispatch {
  class Effectful[F[_]: Monad: Capture: Mods]
  (
    consume: F[Unit] => Unit,
    val state: ObjectProperty[UiState]
  ) extends Dispatch {
    private def syncChanges[A](fa: F[A])(f: (A, RepoState, UiState) => UiState): Unit = consume {
      for {
        a         <- fa
        rState    <- Mods.state
        nextState <- Capture { f(a, rState, state()) }
        _         <- Capture { state.value = nextState }
      } yield ()
    }

    def update(key: Key, diff: Deltas.OfMod): Unit =
      syncChanges(Mods.update(key, diff)) { (_, rs, us) =>
        us
      }

    def remove(key: Key): Unit =
      syncChanges(Mods.remove(key)) { (_, rs, us) =>
        us
      }

    def liftFile(p: Path): Unit =
      syncChanges(Mods.liftFile(p)) { (_, rs, us) =>
        us
      }

    override def closeErrorDialog(): Unit =
      state() = state().clearError


    override def associatePending(k: String, v: Option[String]): Unit = ???

    override def applyPendingAdds(): Unit = ???

    override def cancelPendingAdds(): Unit = ???
  }
}
