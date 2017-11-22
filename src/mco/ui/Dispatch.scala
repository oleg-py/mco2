package mco.ui

import scalaz._
import Id._
import std.option._
import std.vector._

import mco.core.{ImageStore, Mods}
import mco.core.state.{Deltas, RepoState}
import mco.data.{Key, Path}
import mco.util.Capture
import mco.util.syntax.fp._
import mco.core.vars.Var

trait Dispatch {
  protected def state: Var[Id, UiState]

  def setThumbnail(path: String): Unit
  def addPending(paths: Vector[String]): Unit
  def update(key: Key, diff: Deltas.OfMod): Unit
  def remove(key: Key): Unit
  def applyPendingAdds(): Unit
  def setActivePackage(key: Key): Unit

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
  class Effectful[F[_]: Monad: Capture: Mods: ImageStore]
  (runLater: F[Unit] => Unit)(
    protected val state: Var[Id, UiState],
  ) extends Dispatch {
    private def syncChanges[A](fa: F[A])(f: (A, RepoState, UiState) => UiState): Unit =
      runLater {
        for {
          a         <- fa
          rState    <- Mods.state
          nextState <- Capture { f(a, rState, state().copy(rState)) }
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

    override def setThumbnail(path: String): Unit = {
      val op = state().currentModKey.traverse_(key =>
        ImageStore.putImage(key, Some(Path("-os") / path))
      )
      syncChanges(op) { (_, _, us) => us }
    }


    override def setActivePackage(key: Key): Unit = {
      syncChanges(ImageStore.getImage(key)) { (img, _, us) =>
        us.copy(
          currentModKey = Some(key),
          thumbnailUrl = img
        )
      }
    }

    override def addPending(paths: Vector[String]): Unit = {
      val assoc = paths.strengthR(none[String]).toMap
      val mod = UiState.pendingAdds.set(Some(
        UiState.PendingAdds(packages = paths, assoc = assoc)
      ))

      state ~= mod
    }
    override def applyPendingAdds(): Unit = {
      val assocs = UiState.assocL.getOption(state()).getOrElse(Map())

      val op = assocs.keys.toVector.traverse_ { str =>
        Mods.liftFile(Path("-os") / str).void
      }

      syncChanges(op) { (_, rs, us) => us.copy(pendingAdds = None) }
    }
  }
}
