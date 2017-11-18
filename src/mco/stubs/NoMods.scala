package mco.stubs

import scalaz._
import scalaz.std.option._
import scalaz.syntax.applicative._

import mco.core.state.{Deltas, ModState, RepoState}
import mco.core.Mods
import mco.data.{Key, Path}


class NoMods[F[_]: Applicative] extends Mods[F] {
  override def state = RepoState(Vector(), Map()).point[F]
  override def update(key: Key, diff: Deltas.OfMod) = ().point[F]
  override def remove(key: Key) = ().point[F]
  override def liftFile(p: Path) = none[ModState].point[F]
}
