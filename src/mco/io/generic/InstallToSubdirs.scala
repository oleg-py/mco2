package mco.io.generic

import mco.core._
import mco.core.state.ModState
import mco.data.paths._
import mco.data.Keyed

class InstallToSubdirs(target: Path) extends NameResolver
{
  override def apply(index: Int, mod: Keyed[ModState], content: RelPath): Path =
    path"$target/${mod.key}/$content"
}
