package mco.core

import com.olegpy.forwarders
import mco.core.state.{ModState, RepoState}
import mco.data.{Key, Keyed, Path}


@forwarders trait Mods[F[_]] {
  def state: F[RepoState]
  def update(key: Key, diff: Deltas.OfMod): F[Unit]

  /* TODO
  def remove(key: Key): F[Unit]
  def setOrder(key: Key, order: Int): F[Unit]
  def fromFile(p: Path): F[Labelled[ModState]]
  */
}
