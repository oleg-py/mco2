package mco.data

import scalaz._
import syntax.bind._

import mco.io.generic.Filesystem


sealed trait TempOp[F[_], A] extends Product with Serializable {
  import TempOp._
  def runFS(implicit F: Filesystem[F]): F[A] = this match {
    case NoTemp(fa) => fa
    case WithTemp(func) => F.runTmp(func)
  }

  def unify: TempOp.WithTemp[F, A] = this match {
    case NoTemp(fa) => WithTemp(_ => fa)
    case wt @ WithTemp(_) => wt
  }

  def andThen[B](f: A => F[B])(implicit F: Bind[F]): TempOp[F, B] = this match {
    case NoTemp(fa) => NoTemp(fa >>= f)
    case WithTemp(func) => WithTemp(func(_) >>= f)
  }
}

object TempOp {
  case class NoTemp[F[_], A](fa: F[A]) extends TempOp[F, A]
  case class WithTemp[F[_], A](func: Path => F[A]) extends TempOp[F, A]

  def apply[F[_], A](fa: F[A]): TempOp[F, A] = NoTemp(fa)

  implicit def applicative[F[_]: Applicative]: Applicative[TempOp[F, ?]] =
    new Applicative[TempOp[F, ?]] {
      override def point[A](a: => A) = NoTemp(Applicative[F].point(a))

      override def ap[A, B](fa: => TempOp[F, A])(f: => TempOp[F, A => B]): TempOp[F, B] =
        (fa, f) match {
          case (NoTemp(l), NoTemp(r)) => NoTemp(Applicative[F].ap(l)(r))
          case (WithTemp(fl), WithTemp(fr)) => WithTemp { path =>
            Applicative[F].ap(fl(path))(fr(path))
          }
          case (l, r) => ap(l.unify)(r.unify)
        }
    }
}
