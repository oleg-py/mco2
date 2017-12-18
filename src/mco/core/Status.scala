package mco.core

import enumeratum._


sealed trait Status extends EnumEntry

object Status extends Enum[Status] {
  case object Installed extends Status
  case object Unused    extends Status

  val values = findValues
}
