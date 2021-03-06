package mco.ui.state

import cats._
import cats.syntax.all._
import cats.instances.option._
import cats.instances.vector._

import mco.core._
import mco.core.paths.{Path, RelPath}
import mco.core.state.{Deltas, RepoState}
import mco.core.vars.Var


/*_*/
abstract class Commands {
  def setCurrentKind(cKey: RelPath, newValue: ContentKind): Unit =
    syncChanges {
      for {
        st <- tabState()
        mods <- RepoSeq.mods
        ckDelta = Deltas.OfContent(assignedKind = Some(newValue))
        _ <- st.currentModKey
          .traverse_(mods.update(_, Deltas.OfMod(contents = Map(cKey -> ckDelta))))
      } yield ()
    } { (_, _, us) => us }

  type F[_]
  protected val runLater: F[Unit] => Unit
  protected val state: Var[F, UiState]
  implicit protected val monad: Monad[F]
  implicit protected val repoMap: RepoSeq[F]

  private lazy val tabState = state.zoom(UiState.currentTabL)

  private def syncChangesMods[A](ffa: ModStore[F] => F[A]) =
    syncChanges(RepoSeq.mods >>= ffa) _

  private def syncChangesImageStore[A](ffa: ImageStore[F] => F[A]) =
    syncChanges(RepoSeq.imageStore >>= ffa) _

  private def syncChanges[A](fa: F[A])(
    f: (A, RepoState, UiState.Tab) => UiState.Tab): Unit =
    runLater {
      for {
        a         <- fa
        mods      <- RepoSeq.mods
        rState    <- mods.state
        nextState <- tabState().map(st => f(a, rState, st.copy(repoState = rState)))
        _         <- tabState := nextState
      } yield ()
    }

  def setActiveTab(i: Int): Unit = {
    runLater {
      RepoSeq.focus(i) *>
        (state ~= UiState.currentTab.set(i))
    }
  }

  def update(key: RelPath, diff: Deltas.OfMod): Unit =
    syncChangesMods(_.update(key, diff)) { (_, _, us) => us }

  def setThumbnail(path: String): Unit = {
    val op = for {
      st <- tabState()
      keyOpt = st.currentModKey
      imageStore <- RepoSeq.imageStore
      url <- keyOpt.traverse { key =>
        imageStore.putImage(key, Some(Path(path))) *>
          imageStore.getImage(key)
      }
    } yield url.flatten

    syncChanges(op) { (url, _, us) => us.copy(thumbnailUrl = url  ) }
  }

  def unsetThumbnail(): Unit = syncChangesImageStore(store =>
    tabState().flatMap(_.currentModKey.traverse(store.putImage(_, None)))
  ) { (_, _, us) => us.copy(thumbnailUrl = None) }


  def setActivePackage(key: RelPath): Unit = {
    syncChangesImageStore(_.getImage(key)) { (img, _, us) =>
      us.copy(
        currentModKey = Some(key),
        thumbnailUrl = img
      )
    }
  }

  def addPending(paths: Vector[String]): Unit = {
    val assoc = paths.tupleRight(none[String]).toMap
    val mod = UiState.Tab.pendingAdds.set(Some(
      UiState.PendingAdds(packages = paths, assoc = assoc)
    ))

    runLater { tabState ~= mod }
  }

  def applyPendingAdds(): Unit =  {
    val op = for {
      st <- tabState()
      mods <- RepoSeq.mods
      assocs = UiState.Tab.assocL.getOption(st).getOrElse(Map())
      _ <- assocs.keys.toVector.traverse_ { str =>
        mods.liftFile(Path(str)).void
      }
    } yield ()

    syncChanges(op) { (_, _, us) => us.copy(pendingAdds = None) }
  }

  final def install(key: RelPath): Unit =
    update(key, Deltas.OfMod(status = Some(Status.Installed)))

  final def uninstall(key: RelPath): Unit =
    update(key, Deltas.OfMod(status = Some(Status.Unused)))

  final def showNotification(str: String): Unit =
    showError(new Exception(str))

  final def showError(ex: Throwable): Unit =
    runLater { state ~= UiState.error.set(Some(ex)) }

  final def closeErrorDialog(): Unit =
    runLater { state ~= UiState.error.set(None) }

  final def associatePending(k: String, v: Option[String]): Unit =
    runLater { tabState ~= UiState.Tab.assocL.modify(_.updated(k, v)) }

  final def cancelPendingAdds(): Unit =
    runLater { tabState ~= UiState.Tab.pendingAdds.set(None) }

  def toggleActive(): Unit = syncChangesMods { mods =>
    for {
      st <- tabState()
      keyO = st.currentModKey
      modO = st.currentMod
      _ <- (modO, keyO).tupled.traverse_ { case (modState, path) =>
        modState.status match {
          case Status.Installed =>
            mods.update(path, Deltas.OfMod(status = Some(Status.Unused)))
          case Status.Unused =>
            mods.update(path, Deltas.OfMod(status = Some(Status.Installed)))
        }
      }
    } yield ()
  } { (_, _, us) => us }


  def removeActive(): Unit = syncChangesMods { mods =>
    for {
      st <- tabState()
      _  <- st.currentModKey.traverse_(mods.remove)
    } yield ()
  } { (_, _, us) => us }
}

object Commands {
  def apply[F0[_]: Monad](
    map: RepoSeq[F0],
    runLater0: F0[Unit] => Unit,
    state0: Var[F0, UiState]
  ): Commands = new Commands {
      override type F[A] = F0[A]
      override protected val runLater: F[Unit] => Unit = runLater0
      override protected val state: Var[F, UiState] = state0
      override protected val monad: Monad[F] = Monad[F0]
      override protected val repoMap: RepoSeq[F] = map
  }
}
