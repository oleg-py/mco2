package mco.core

import mco.core.paths._
import mco.core.state.{Deltas, ModState, RepoState}


/**
 * Algebra of operations over a set of mods
 * @tparam F - the effect type
 */
trait ModStore[F[_]] {
  /**
   * Read current state of repository
   */
  def state: F[RepoState]

  /**
   * Update mod inside a repository with provided data
   * @param key the identifier of the mod
   * @param diff the changes to be applied to the mod
   */
  def update(key: RelPath, diff: Deltas.OfMod): F[Unit]

  /**
   * Remove mod from this repository
   * @param key the identifier for the mod
   */
  def remove(key: RelPath): F[Unit]

  /**
   * Attempt to add provided file to the repository
   * if it can be treated as a a mod
   *
   * @param path the file location
   * @return an option that is nonempty if there
   *         was no error lifting the file
   */
  def liftFile(path: Path): F[ModState]

  /* TODO
  def setOrder(key: Key, order: Int): F[Unit]
  */
}
