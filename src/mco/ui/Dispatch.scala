package mco.ui

import scalaz.Id._

import mco.core.Mods
import mco.core.state.{Deltas, RepoState}
import mco.data.Key
import mco.util.Capture
import scalaz.Monad
import scalaz.syntax.monad._

import mco.core.vars.Var

trait Dispatch {
  protected def state: Var[Id, UiState]

  def setThumbnail(path: String): Unit
  def liftPackages(paths: Vector[String]): Unit
  def update(key: Key, diff: Deltas.OfMod): Unit
  def remove(key: Key): Unit
  def applyPendingAdds(): Unit

  final def installActive(): Unit =
    state().currentModKey.foreach(install)

  final def uninstallActive(): Unit =
    state().currentModKey.foreach(uninstall)

  final def setLabel(key: Key, label: String): Unit =
    update(key, Deltas.OfMod(label = Some(label)))

  final def install(key: Key): Unit =
    update(key, Deltas.OfMod(enabled = Some(true)))

  final def uninstall(key: Key): Unit =
    update(key, Deltas.OfMod(enabled = Some(false)))

  final def setActivePackage(key: Key): Unit =
    state ~= UiState.currentModKey.set(Some(key))

  final def showNotification(str: String): Unit =
    ???

  final def closeErrorDialog(): Unit =
    state ~= UiState.error.set(None)

  final def associatePending(k: String, v: Option[String]): Unit =
    state ~= UiState.assocL.modify(_.updated(k, v))

  final def cancelPendingAdds(): Unit =
    state ~= UiState.pendingAdds.set(None)
}

object Dispatch {
  class Effectful[F[_]: Monad: Capture: Mods]
  (runLater: F[Unit] => Unit)(
    protected val state: Var[Id, UiState],
  ) extends Dispatch {
    private def syncChanges[A](fa: F[A])(f: (A, RepoState, UiState) => UiState): Unit =
      runLater {
        for {
          a         <- fa
          rState    <- Mods.state
          nextState <- Capture { f(a, rState, state()) }
          _         <- Capture { state := nextState }
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

    override def setThumbnail(path: String): Unit = ???
    override def liftPackages(paths: Vector[String]): Unit = ???
    override def applyPendingAdds(): Unit = ???
  }
}
