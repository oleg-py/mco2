package mco.core

import scalaz._
import scalaz.std.vector._

import mco.core.state.RepoState
import mco.core.vars.Var
import mco.stubs.{NoImageStore, NoMods}
import mco.util.syntax.fp._


trait ModStore[F[_]] {
  def labels: Vector[String]
  def length: Int

  def states: F[Vector[RepoState]]
  def mods: F[Mods[F]]
  def imageStore: F[ImageStore[F]]
  def focus(i: Int): F[Unit]
}

object ModStore {
  class ByVar[F[_]: Applicative](
    algebras: Vector[(String, Mods[F], ImageStore[F])],
    selection: Var[F, Int]
  ) extends ModStore[F] {
    private[this] def tuple = selection().map(algebras(_))

    override val labels: Vector[String]       = algebras.map(_._1)
    override val length: Int                  = algebras.length

    override def states: F[Vector[RepoState]] =
      algebras.traverse { case (_, mods, _) => mods.state }

    override def mods: F[Mods[F]]             = tuple.map(_._2)
    override def imageStore: F[ImageStore[F]] = tuple.map(_._3)
    override def focus(i: Int): F[Unit]       = selection := i
  }

  class Empty[F[_]: Applicative] extends ModStore[F] {
    override def labels: Vector[String] = Vector.empty
    override def length: Int = 0

    override def mods: F[Mods[F]] = new NoMods[F].point[F].widen

    override def states: F[Vector[RepoState]] = Vector.empty[RepoState].point[F]

    override def imageStore: F[ImageStore[F]] = new NoImageStore[F].point[F].widen
    override def focus(i: Int): F[Unit] = ().point[F]
  }
}
