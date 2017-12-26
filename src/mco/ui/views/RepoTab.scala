package mco.ui.views

import scalafx.Includes._
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.control.{SplitPane, Tab}
import scalafx.scene.layout._

import mco.ui._
import mco.ui.props._
import mco.ui.state.{Commands, UiState}
import cats.syntax.all._

class RepoTab(state: Prop[UiState.Tab], isImage: Prop[String => Boolean])(
  implicit cmd: Commands)
  extends Tab
{
  text <== state.map(_.label)

  closable = false
  content = new SplitPane { contentRoot =>
    padding = Insets(4)

    private val mainView = Seq(
      new RepoPackagesTable(state.map(_.repoState)),
      new VBox {
        padding = Insets(0, 0, 0, 10)
        prefWidth <== (contentRoot.width / 3)
        children = Seq(
          new SplitPane {
            vgrow = Priority.Always

            orientation = Orientation.Vertical
            items ++= Seq(
              new PackageThumbnail(
                state.map(_.currentMod.nonEmpty),
                state.map(_.thumbnailUrl),
                isImage),
              new PackageContentTable(state.map(_.currentContent)),
            )
          },
          new PackageActionButtons(state.map(_.currentMod))
        )
      }
    )

    private val adds = state.map(_.pendingAdds.getOrElse(UiState.PendingAdds()))
    private lazy val pendingAddsView = Seq(new BulkAssoc(adds))

    items <== state.map { s =>
      if (s.pendingAdds.isEmpty) mainView
      else pendingAddsView
    }
  }
}
