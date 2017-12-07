package mco.stubs

import cats._
import cats.syntax.option._
import cats.syntax.applicative._

import mco.core.state.{Deltas, ModState, RepoState}
import mco.core.Mods
import mco.core.paths._


class NoMods[F[_]: Applicative] extends Mods[F] {
  override def state = RepoState(Vector(), Map()).pure[F]
  override def update(key: RelPath, diff: Deltas.OfMod) = ().pure[F]
  override def remove(key: RelPath) = ().pure[F]
  override def liftFile(p: Path) = none[ModState].pure[F]
}
