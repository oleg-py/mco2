package mco.io.generic

import mco.core._
import mco.core.state.ModState
import mco.data.paths._
import mco.data.{Key, Keyed}

class InstallToSubdirs(target: Path) extends NameResolver
{
  override def apply(index: Int, mod: Keyed[ModState], content: Key): Path =
    path"$target/${mod.key.unwrap}/${content.unwrap}"
}
