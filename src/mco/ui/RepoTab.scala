package mco.ui

import scalafx.geometry.Insets
import scalafx.scene.control.Tab
import scalafx.scene.layout.{HBox, VBox}
import mco.ui.props._
import mco.util.syntax.fp._

class RepoTab(state: Prop[UiState])(implicit dispatch: Dispatch) extends Tab {
  text = "Stub Repo Tab"
  closable = false
  content = new HBox { contentRoot =>
    padding = Insets(10)

    private val mainView = Seq(
//      RepoPackagesTable(state),
      new VBox {
        padding = Insets(0, 0, 0, 10)
        prefWidth <== (contentRoot.width / 3)
        children = Seq(
//          PackageThumbnail(state.map(_.thumbnailURL) product width, act),
//          PackageContentTable(state.map(_.currentPackage.map(_.contents).getOrElse(Set())), act),
//          PackageActionButtons(state.map(_.currentPackage), act)
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
