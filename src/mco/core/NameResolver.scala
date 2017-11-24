package mco.core

import mco.core.state.ModState
import mco.data.paths.{Path, RelPath}
import mco.data.Keyed


trait NameResolver {
  def apply(index: Int, mod: Keyed[ModState], content: RelPath): Path

  final def bulk(
    index: Int,
    modInfo: Keyed[ModState]
  )(targets: Vector[Keyed[Path]]
  ): Vector[Keyed[(Path, Path)]] =
    targets map { case Keyed(key, from) =>
      Keyed(key, from -> this(index, modInfo, key))
    }
}
