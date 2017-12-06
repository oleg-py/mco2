package mco.stubs

import scalaz._
import scalaz.std.option._
import scalaz.syntax.applicative._

import mco.core.state.{Deltas, ModState, RepoState}
import mco.core.Mods
import mco.core.paths._


class NoMods[F[_]: Applicative] extends Mods[F] {
  override def state = RepoState(Vector(), Map()).point[F]
  override def update(key: RelPath, diff: Deltas.OfMod) = ().point[F]
  override def remove(key: RelPath) = ().point[F]
  override def liftFile(p: Path) = none[ModState].point[F]
}
