package mco.core

import scala.util.Random
import scalaz._
import std.anyVal._
import std.option._
import std.tuple._

import mco.core.state.ModState
import mco.data.paths._
import mco.data.Keyed
import mco.util.misc.uuidName
import mco.util.syntax.fp._


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

object NameResolver {
  // this is pure
  private def indexToId(i: Int) =
    new Random(i).nextInt().toHexString

  def mangle(target: Path): NameResolver = (index, mod, content) => {
    val ext = content.extension
    val id = indexToId(index)
    val hashes = mod.get.contents.lookup(content).foldMap(_.stamp.hash)
    path"$target/$id-${uuidName(hashes)}$ext"
  }

  def subdirs(target: Path): NameResolver = (_, mod, content) =>
    path"$target/${mod.key}/$content"
}