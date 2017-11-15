package mco.io.generic

import scalaz._
import syntax.applicative._
import std.option._

import mco.core.state.{ModState, RepoState}
import mco.core.{Deltas, Mods}
import mco.data.{Key, Path}


class DummyMods[F[_]: Applicative] extends Mods[F] {
  override def state = RepoState(Vector()).point[F]
  override def update(key: Key, diff: Deltas.OfMod) = ().point[F]
  override def remove(key: Key) = ().point[F]
  override def liftFile(p: Path) = none[ModState].point[F]
}
