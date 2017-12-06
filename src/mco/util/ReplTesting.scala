package mco.util

import scalaz._
import scalaz.Scalaz._

import better.files._
import mco.core._
import mco.game.generic.{GenericConfig, PrototypeImplementation}
import pureconfig.loadConfig

//noinspection TypeAnnotation
object ReplTesting {
  val Path = paths.Path
  val Deltas = mco.core.state.Deltas

  implicit val yolo = Capture.yolo.captureOfId
  val osRoot = Path("-os")
  val data = {
    implicit val yoloMonadError = new MonadError[Id, Throwable] {
      override def raiseError[A](e: Throwable): A = throw e
      override def handleError[A](fa: Scalaz.Id[A])(f: Throwable => Scalaz.Id[A]): A  =
        fa

      override def bind[A, B](fa: Scalaz.Id[A])(f: A => Scalaz.Id[B]): B  =
        f(fa)
      override def point[A](a: => A) = a
    }

    PrototypeImplementation.algebras[Id](loadConfig[GenericConfig].right.get, Path(
      file".".pathAsString
    ))
  }
}
