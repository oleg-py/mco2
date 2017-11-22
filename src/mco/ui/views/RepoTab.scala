package mco.ui.views

import scalafx.geometry.Insets
import scalafx.scene.control.Tab
import scalafx.scene.layout.{HBox, VBox}

import mco.ui._
import mco.ui.props._
import mco.util.syntax.fp._

class RepoTab(state: Prop[UiState])(implicit dispatch: Dispatch) extends Tab {
  text = "Stub Repo Tab"
  closable = false
  content = new HBox { contentRoot =>
    padding = Insets(10)

    private val mainView = Seq(
      new RepoPackagesTable(state.map(_.repoState)),
      new VBox {
        padding = Insets(0, 0, 0, 10)
        prefWidth <== (contentRoot.width / 3)
        children = Seq(
          new PackageThumbnail(state.map(_.thumbnailUrl), width),
          new PackageContentTable(state.map(_.currentContent)),
          new PackageActionButtons(state.map(_.currentMod))
        )
      }
    )

    private val adds = state.map(_.pendingAdds.getOrElse(UiState.PendingAdds()))
    private lazy val pendingAddsView = Seq(new BulkAssoc(adds))

    children <== state.map { s =>
      if (s.pendingAdds.isEmpty) mainView
      else pendingAddsView
    }
  }
}
