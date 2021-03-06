package mco.stubs

import cats._
import cats.syntax.option._
import cats.syntax.applicative._

import mco.core.state.{Deltas, ModState, RepoState}
import mco.core.ModStore
import mco.core.paths._


/**
 * Mods algebra that is pure and represents empty
 * collection of mods for any Applicative
 */
class EmptyModStore[F[_]](implicit F: ApplicativeError[F, Throwable]) extends ModStore[F] {
  override def state = RepoState(Vector(), Map()).pure[F]
  override def update(key: RelPath, diff: Deltas.OfMod) = ().pure[F]
  override def remove(key: RelPath) = ().pure[F]
  override def liftFile(p: Path) = F.raiseError(new Exception("Not allowed for stub"))
}
