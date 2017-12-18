package mco.ui.views

import cats.syntax.functor._
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.Button
import scalafx.scene.layout.{HBox, Priority, Region}

import mco.core.Status
import mco.core.state.ModState
import mco.ui.props._
import mco.ui.state.Commands

class PackageActionButtons(state: Prop[Option[ModState]])(implicit cmd: Commands) extends HBox {
  hgrow = Priority.Always
  alignmentInParent = Pos.CenterRight
  padding = Insets(10)
  spacing = 10
  children = Seq(
    new Region {
      minWidth = 10
      maxWidth = Double.MaxValue
      hgrow = Priority.Always
    },
    new Button("View README") {
      prefWidth = 125
      disable = true // TODO implement readmes
    },
    new Button {
      prefWidth = 125
      text <== state.map {
        case Some(p) if p.status == Status.Installed => "Uninstall"
        case _ => "Install"
      }
      disable <== state.map(_.isEmpty)
      onAction = handle {
        for (value <- state()) {
          if (value.status == Status.Installed) cmd.uninstallActive()
          else cmd.installActive()
        }
      }
      defaultButton = true
    }
  )
}
