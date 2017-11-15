package mco.util

import scalafx.beans.value.ObservableValue


trait PropsImplementation {
  type Prop[A] >: ObservableValue[A, _] <: ObservableValue[A, _]

  sealed trait ToJavaFxAs[A, J]

  object ToJavaFxAs extends ToJavaFxAsLowPriority {
    implicit val boolean : Boolean ToJavaFxAs java.lang.Boolean = null
    implicit val double  : Double  ToJavaFxAs Number  = null
    implicit val float   : Float   ToJavaFxAs Number  = null
    implicit val int     : Int     ToJavaFxAs Number  = null
    implicit val long    : Long    ToJavaFxAs Number  = null
  }

  sealed trait ToJavaFxAsLowPriority {
    implicit def any[A]: A ToJavaFxAs A = null
  }
}
