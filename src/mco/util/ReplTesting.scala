package mco.util

import scalaz._
import Scalaz._

import better.files._
import mco.core.Mods
import mco.io.generic.PrototypeImplementation

//noinspection TypeAnnotation
object ReplTesting {
  val Key = mco.data.Key
  val Path = mco.data.Path
  val Deltas = mco.core.state.Deltas

  implicit val yolo = Capture.yolo.captureOfId
  val osRoot = Path("-os")
  val algebra: Mods[Id] = {
    val tmp = PrototypeImplementation.algebra(Path(
      file".".pathAsString
    ))
    tmp: Mods[Id]
  }
}
