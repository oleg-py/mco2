package mco.core

import scalaz._
import std.vector._

import mco.util.syntax.fp._
import mco.core.state.ModState
import mco.data.{Key, Keyed, Path}


trait NameResolver {
  def apply(index: Int, mod: Keyed[ModState], content: Key): Path

  final def bulk(
    index: Int,
    modInfo: Keyed[ModState]
  )(targets: Vector[Keyed[Path]]
  ): Vector[Keyed[(Path, Path)]] =
    targets map { case Keyed(key, from) =>
      Keyed(key, from -> this(index, modInfo, key))
    }
}
