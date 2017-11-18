package mco.core

import scalaz.\/

import com.olegpy.forwarders
import mco.core.state.{Deltas, ModState, RepoState}
import mco.data.{Key, Keyed, Path}


@forwarders trait Mods[F[_]] {
  def state: F[RepoState]
  def update(key: Key, diff: Deltas.OfMod): F[Unit]

  def remove(key: Key): F[Unit]
  def liftFile(p: Path): F[Option[ModState]]
  /* TODO
  def setOrder(key: Key, order: Int): F[Unit]
  */
}
