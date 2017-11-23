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

/*_*/
abstract class Commands {
  type F[_]
  protected val runLater: F[Unit] => Unit
  protected val state: Var[Id, UiState]
  implicit val ev1: Monad[F]
  implicit val ev2: Capture[F]
  implicit val ev3: Mods[F]
  implicit val ev4: ImageStore[F]

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

  def setThumbnail(path: String): Unit = {
    val op = state().currentModKey.traverse(key =>
      ImageStore.putImage(key, Some(Path("-os") / path))
        >> ImageStore.getImage(key)
    )
    syncChanges(op) { (url, _, us) => us.copy(thumbnailUrl = url.flatten) }
  }


  def setActivePackage(key: Key): Unit = {
    syncChanges(ImageStore.getImage(key)) { (img, _, us) =>
      us.copy(
        currentModKey = Some(key),
        thumbnailUrl = img
      )
    }
  }

  def addPending(paths: Vector[String]): Unit = {
    val assoc = paths.strengthR(none[String]).toMap
    val mod = UiState.pendingAdds.set(Some(
      UiState.PendingAdds(packages = paths, assoc = assoc)
    ))

    state ~= mod
  }
  def applyPendingAdds(): Unit = {
    val assocs = UiState.assocL.getOption(state()).getOrElse(Map())

    val op = assocs.keys.toVector.traverse_ { str =>
      Mods.liftFile(Path("-os") / str).void
    }

    syncChanges(op) { (_, rs, us) => us.copy(pendingAdds = None) }
  }

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

object Commands {
  def apply[F0[_]: Monad: Capture: Mods: ImageStore](runLater0: F0[Unit] => Unit)(state0: Var[Id, UiState]):
    Commands = new Commands {
      override type F[A] = F0[A]
      override protected val runLater: F[Unit] => Unit = runLater0
      override protected val state: Var[scalaz.Id.Id, UiState] = state0
      override val ev1: Monad[F] = implicitly
      override val ev2: Capture[F] = implicitly
      override val ev3: Mods[F] = implicitly
      override val ev4: ImageStore[F] = implicitly
  }
}
