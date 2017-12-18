package mco.core

import enumeratum._


sealed trait ContentKind extends EnumEntry

object ContentKind extends Enum[ContentKind] {
  case object Document   extends ContentKind
  case object Component  extends ContentKind
  case object Unused     extends ContentKind

  val values = findValues
}
