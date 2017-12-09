package mco.ui.state

import mco.core.paths.RelPath
import mco.core.state.{ContentState, ModState, RepoState}
import mco.syntax._
import monocle.macros.Lenses
import monocle.std.option.{some => someL}
import monocle.{Lens, Optional}

import java.net.URL

@Lenses case class UiState(
  tabs: Vector[UiState.Tab] = Vector(),
  isImage: String => Boolean = _ => false,
  currentTab: Int = 0,
  error: Option[Throwable] = None
)

object UiState {
  val currentTabL = Lens[UiState, Tab](
    us => us.tabs(us.currentTab))(
    tab => us => UiState.tabs.modify(_.updated(us.currentTab, tab))(us))

  @Lenses case class Tab (
    label: String,
    repoState: RepoState = RepoState(),
    pendingAdds: Option[PendingAdds] = None,
    currentModKey: Option[RelPath] = None,
    thumbnailUrl: Option[URL] = None,
  ) {
    def currentMod: Option[ModState] = for {
      key <- currentModKey
      mod <- repoState.orderedMods.find(_.key == key)
    } yield mod.get

    def currentContent: Vector[(String, ContentState)] =
      currentMod
        .map(_.contents.mapKeys(_.toString).toVector)
        .getOrElse(Vector())
  }

  object Tab {
    val assocL: Optional[Tab, Map[String, Option[String]]] =
      Tab.pendingAdds
        .composePrism(someL)
        .composeLens(PendingAdds.assoc)
  }

  @Lenses case class PendingAdds (
    packages: Vector[String] = Vector(),
    images: Vector[String] = Vector(),
    assoc: Map[String, Option[String]] = Map()
  )

  def startupError(throwable: Throwable) =
    UiState(error = Some(throwable))

  def initial(repoStates: Vector[(String, RepoState)], isImage: String => Boolean) =
    UiState(
      repoStates.map { case (label, state) => Tab(label, state) },
      isImage
    )
}
