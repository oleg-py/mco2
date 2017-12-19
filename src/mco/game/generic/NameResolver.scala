package mco.game.generic

import scala.util.Random

import mco.core.paths._


trait NameResolver {
  def apply(mod: RelPath, content: RelPath): Path

  final def bulk(
    modInfo: RelPath
  )(targets: Vector[Pointed[Path]]
  ): Vector[Pointed[(Path, Path)]] =
    targets map { case (key, from) =>
      (key, from -> this(modInfo, key))
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

  def subdirs(target: Path): NameResolver = (parent, content) =>
    path"$target/$parent/$content"

  def overrides(target: Path): NameResolver = (_, content) =>
    path"$target/$content"
}