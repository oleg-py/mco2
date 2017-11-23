package mco.io.generic

import mco.core._
import scalaz._
import syntax.std.option._
import syntax.applicative._

import mco.core.state.ModState
import mco.data.{Key, Keyed, Path}

class InstallToSubdirs(target: Path) extends NameResolver
{
  override def apply(index: Int, mod: Keyed[ModState], content: Key): Path =
    target / mod.key.unwrap / content.unwrap
}
