package mco.util

import scalaz.{Scalaz, _}
import scalaz.Scalaz._

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
    implicit val yoloMonadError = new MonadError[Id, Throwable] {
      override def raiseError[A](e: Throwable): A = throw e
      override def handleError[A](fa: Scalaz.Id[A])(f: Throwable => Scalaz.Id[A]): A  =
        fa

      override def bind[A, B](fa: Scalaz.Id[A])(f: A => Scalaz.Id[B]): B  =
        f(fa)
      override def point[A](a: => A) = a
    }

    val tmp = PrototypeImplementation.algebra(Path(
      file".".pathAsString
    ))
    tmp: Mods[Id]
  }
}
