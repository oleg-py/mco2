package mco.core

import enumeratum._


/**
 * Enum of roles content inside mod can be treated as
 */
sealed trait ContentKind extends EnumEntry

object ContentKind extends Enum[ContentKind] {
  case object Document   extends ContentKind
  case object Component  extends ContentKind
  case object Unused     extends ContentKind

  val values = findValues
}
