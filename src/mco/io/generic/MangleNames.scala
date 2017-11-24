package mco.io.generic

import scala.util.Random
import scalaz._
import std.anyVal._
import std.option._
import std.tuple._

import mco.core._
import mco.core.state.ModState
import mco.data.paths._
import mco.data.Keyed
import mco.util.syntax.fp._
import mco.util.misc.uuidName

class MangleNames(target: Path) extends NameResolver
{
  // Random is pure if using seed
  private def indexToId(i: Int) =
    new Random(i).nextInt().toHexString


  override def apply(index: Int, mod: Keyed[ModState], content: RelPath): Path = {
    val ext = content.extension
    val id = indexToId(index)
    val hashes = mod.get.contents.lookup(content).foldMap(_.stamp.hash)
    path"$target/$id-${uuidName(hashes)}$ext"
  }
}
