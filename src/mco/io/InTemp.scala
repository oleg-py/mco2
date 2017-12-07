package mco.io

import cats._
import syntax.functor._
import syntax.flatMap._

import mco.core.paths._


sealed trait InTemp[F[_], A] extends Product with Serializable {
  import InTemp._
  // primarily to make IDEA happier
  def map[B](f: A => B)(implicit F: Functor[F]): InTemp[F, B] = this match {
    case NoTemp(fa) => NoTemp(fa map f)
    case WithTemp(ffa) => WithTemp(ffa andThen F.lift(f))
  }

  def runFS(implicit F: Filesystem[F]): F[A] = this match {
    case NoTemp(fa) => fa
    case WithTemp(func) => F.runTmp(func)
  }

  def unify: InTemp.WithTemp[F, A] = this match {
    case NoTemp(fa) => WithTemp(_ => fa)
    case wt @ WithTemp(_) => wt
  }

  def andThen[B](f: A => F[B])(implicit F: FlatMap[F]): InTemp[F, B] = this match {
    case NoTemp(fa) => NoTemp(fa >>= f)
    case WithTemp(func) => WithTemp(func(_) >>= f)
  }
}

object InTemp {
  case class NoTemp[F[_], A](fa: F[A]) extends InTemp[F, A]
  case class WithTemp[F[_], A](func: Path => F[A]) extends InTemp[F, A]

  def apply[F[_], A](fa: F[A]): InTemp[F, A] = NoTemp(fa)

  implicit def applicative[F[_]: Applicative]: Applicative[InTemp[F, ?]] =
    new Applicative[InTemp[F, ?]] {
      override def pure[A](a: A) = NoTemp(Applicative[F].pure(a))

      override def ap[A, B](f: InTemp[F, A => B])(fa: InTemp[F, A]): InTemp[F, B] =
        (f, fa) match {
          case (NoTemp(l), NoTemp(r)) => NoTemp(Applicative[F].ap(l)(r))
          case (WithTemp(fl), WithTemp(fr)) => WithTemp { path =>
            Applicative[F].ap(fl(path))(fr(path))
          }
          case (l, r) => ap(l.unify)(r.unify)
        }
    }
}
