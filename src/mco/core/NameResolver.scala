package mco.core

import scala.util.Random

import mco.core.paths._
import mco.core.state.ModState


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

  // TODO: decide if feasible
//  def mangle(target: Path): NameResolver = (mod, content) => {
//    val ext = content.extension
//    val id = indexToId(mod.key.##)
//    val hashes = mod.get.contents.lookup(content).foldMap(_.stamp.hash)
//    path"$target/$id-${uuidName(hashes)}$ext"
//  }

  def subdirs(target: Path): NameResolver = (mod, content) =>
    path"$target/${mod.key}/$content"

  def overrides(target: Path): NameResolver = (_, content) =>
    path"$target/$content"
}