package mco.core.vars

import boopickle.Default._
import cats.effect.Sync
import cats.syntax.all._
import mco.core.paths.Path
import mco.io.Filesystem

class BoopickleVar[F[_]: Filesystem: Sync, A: Pickler](
  target: Path
) extends Var[F, A]{
  override def apply(): F[A] =
    Filesystem[F].readFile(target)
      .map(Unpickle[A].fromBytes)
      .runLastSync
      .map(_.getOrElse(sys.error("ReadFile returned nothing")))

  override def :=(a: A): F[Unit] =
    Filesystem[F].writeFile(target, Pickle.intoBytes(a))
}
