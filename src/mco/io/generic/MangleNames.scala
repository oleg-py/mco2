package mco.io.generic

import scala.util.Random
import scalaz._
import std.anyVal._
import std.option._
import std.tuple._

import mco.core._
import mco.core.state.ModState
import mco.data.{Key, Keyed, Path}
import mco.util.syntax.fp._
import mco.util.misc.uuidName

class MangleNames(target: Path) extends NameResolver
{
  private val step = (1L, 3L)

  // Random is pure if using seed
  private def indexToId(i: Int) =
    new Random(i).nextInt().toHexString


  override def apply(index: Int, mod: Keyed[ModState], content: Key): Path = {
    val ext = (Path.root / content.unwrap).extension
    val id = indexToId(index)
    val hashes = mod.get.contents.lookup(content).foldMap(_.stamp.hash)
    target / s"$id-${uuidName(hashes)}$ext"
  }
}
