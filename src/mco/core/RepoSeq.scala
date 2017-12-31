package mco.core

import cats._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.instances.vector._
import com.olegpy.forwarders
import mco.core.state.RepoState
import mco.core.vars.Var
import mco.stubs.{NoImageStore, EmptyModStore}


/**
 * Algebra of operations over a sequence of mod & image stores
 * where one element is considered "current" (set using `focus`
 * operation)
 *
 *
 * @tparam F the effect type
 */
@forwarders trait RepoSeq[F[_]] {
  def labels: Vector[String]
  def length: Int

  def states: F[Vector[RepoState]]
  def mods: F[ModStore[F]]
  def imageStore: F[ImageStore[F]]
  def focus(i: Int): F[Unit]
}

object RepoSeq {
  class ByVar[F[_]: Applicative](
    algebras: Vector[(String, ModStore[F], ImageStore[F])],
    selection: Var[F, Int]
  ) extends RepoSeq[F] {
    private[this] def tuple = selection().map(algebras(_))

    override val labels: Vector[String]       = algebras.map(_._1)
    override val length: Int                  = algebras.length

    override def states: F[Vector[RepoState]] =
      algebras.traverse { case (_, mods, _) => mods.state }

    override def mods: F[ModStore[F]]             = tuple.map(_._2)
    override def imageStore: F[ImageStore[F]] = tuple.map(_._3)
    override def focus(i: Int): F[Unit]       = selection := i
  }

  class Empty[F[_]: ApplicativeError[?[_], Throwable]] extends RepoSeq[F] {
    override def labels: Vector[String] = Vector.empty
    override def length: Int = 0
    override def mods: F[ModStore[F]] = new EmptyModStore[F].pure[F].widen
    override def states: F[Vector[RepoState]] = Vector.empty[RepoState].pure[F]
    override def imageStore: F[ImageStore[F]] = new NoImageStore[F].pure[F].widen
    override def focus(i: Int): F[Unit] = ().pure[F]
  }
}
