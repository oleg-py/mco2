package mco.core

import scala.util.Random
import scalaz._
import std.anyVal._
import std.option._
import std.tuple._

import mco.core.paths._
import mco.core.state.ModState
import mco.util.misc.uuidName
import mco.util.syntax.fp._


trait NameResolver {
  def apply(mod: Pointed[ModState], content: RelPath): Path

  final def bulk(
    modInfo: Pointed[ModState]
  )(targets: Vector[Pointed[Path]]
  ): Vector[Pointed[(Path, Path)]] =
    targets map { case Pointed(key, from) =>
      Pointed(key, from -> this(modInfo, key))
    }
}

object NameResolver {
  // this is pure
  private def indexToId(i: Int) =
    new Random(i).nextInt().toHexString

  def mangle(target: Path): NameResolver = (mod, content) => {
    val ext = content.extension
    val id = indexToId(mod.key.##)
    val hashes = mod.get.contents.lookup(content).foldMap(_.stamp.hash)
    path"$target/$id-${uuidName(hashes)}$ext"
  }

  def subdirs(target: Path): NameResolver = (mod, content) =>
    path"$target/${mod.key}/$content"

  def overrides(target: Path): NameResolver = (_, content) =>
    path"$target/$content"
}