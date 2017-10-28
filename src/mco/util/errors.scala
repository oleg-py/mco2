package mco.util

import scalaz._

object errors {
  sealed trait UserErr {
    def message: String
  }

  type ErrOr[A] = ValidationNel[UserErr, A]
}
