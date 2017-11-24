package mco.ui

import scalaz._
import Id._
import std.option._
import std.vector._

import mco.core.{Capture, ImageStore, Mods}
import mco.core.state.{Deltas, RepoState}
import mco.data.paths._
import mco.util.syntax.fp._
import mco.core.vars.Var

/*_*/
abstract class Commands {
  type F[_]
  protected val runLater: F[Unit] => Unit
  protected val state: Var[F, UiState]
  implicit val ev1: Monad[F]
  implicit val ev3: Mods[F]
  implicit val ev4: ImageStore[F]

  private def syncChanges[A](fa: F[A])(f: (A, RepoState, UiState) => UiState): Unit =
    runLater {
      for {
        a         <- fa
        rState    <- Mods.state
        nextState <- state().map(st => f(a, rState, st.copy(rState)))
        _         <- state := nextState
      } yield ()
    }

  def update(key: RelPath, diff: Deltas.OfMod): Unit =
    syncChanges(Mods.update(key, diff)) { (_, _, us) => us }

  def remove(key: RelPath): Unit =
    syncChanges(Mods.remove(key)) { (_, _, us) => us }

  def setThumbnail(path: String): Unit = {
    val op = state().flatMap(_.currentModKey.traverse(key =>
      ImageStore.putImage(key, Some(Path("-os") / RelPath(path)))
        >> ImageStore.getImage(key)
    ))
    syncChanges(op) { (url, _, us) => us.copy(thumbnailUrl = url.flatten) }
  }


  def setActivePackage(key: RelPath): Unit = {
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

    runLater { state ~= mod }
  }

  def applyPendingAdds(): Unit =  {
    val op = for {
      st <- state()
      assocs = UiState.assocL.getOption(st).getOrElse(Map())
      _ <- assocs.keys.toVector.traverse_ { str =>
        Mods.liftFile(Path("-os") / RelPath(str)).void
      }
    } yield ()

    syncChanges(op) { (_, rs, us) => us.copy(pendingAdds = None) }
  }

  final def installActive(): Unit = syncChanges {
    for {
      st <- state()
      _ <- st.currentModKey.traverse_(Mods.update(_, Deltas.OfMod(enabled = Some(true))))
    } yield ()
  } { (_, _, us) => us }

  final def uninstallActive(): Unit = syncChanges {
    for {
      st <- state()
      _ <- st.currentModKey.traverse_(Mods.update(_, Deltas.OfMod(enabled = Some(false))))
    } yield ()
  } { (_, _, us) => us }

  final def setLabel(key: RelPath, label: String): Unit =
    update(key, Deltas.OfMod(label = Some(label)))

  final def install(key: RelPath): Unit =
    update(key, Deltas.OfMod(enabled = Some(true)))

  final def uninstall(key: RelPath): Unit =
    update(key, Deltas.OfMod(enabled = Some(false)))

  final def showNotification(str: String): Unit =
    ???

  final def closeErrorDialog(): Unit =
    runLater { state ~= UiState.error.set(None) }

  final def associatePending(k: String, v: Option[String]): Unit =
    runLater { state ~= UiState.assocL.modify(_.updated(k, v)) }

  final def cancelPendingAdds(): Unit =
    runLater { state ~= UiState.pendingAdds.set(None) }
}

object Commands {
  def apply[F0[_]: Monad: Mods: ImageStore](runLater0: F0[Unit] => Unit)(state0: Var[F0, UiState]):
    Commands = new Commands {
      override type F[A] = F0[A]
      override protected val runLater: F[Unit] => Unit = runLater0
      override protected val state: Var[F, UiState] = state0
      override val ev1: Monad[F] = implicitly
      override val ev3: Mods[F] = implicitly
      override val ev4: ImageStore[F] = implicitly
  }
}
