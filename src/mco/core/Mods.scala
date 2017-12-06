package mco.core

import com.olegpy.forwarders
import mco.core.paths._
import mco.core.state.{Deltas, ModState, RepoState}


@forwarders trait Mods[F[_]] {
  def state: F[RepoState]
  def update(key: RelPath, diff: Deltas.OfMod): F[Unit]

  def remove(key: RelPath): F[Unit]
  def liftFile(p: Path): F[Option[ModState]]
  /* TODO
  def setOrder(key: Key, order: Int): F[Unit]
  */
}
