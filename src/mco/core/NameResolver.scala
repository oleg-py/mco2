package mco.core

import scalaz._
import std.vector._

import mco.util.syntax.fp._
import mco.core.state.ModState
import mco.data.{Key, Keyed, Path}


trait NameResolver[F[_]] {
  def apply(mod: Keyed[ModState])(content: Key): F[Option[Path]]

  final def bulk(
    modInfo: Keyed[ModState]
  )(targets: Vector[Keyed[Path]]
  )(implicit F: Monad[F]): F[Vector[(Path, Keyed[Option[Path]])]] = {
    val resolveMissing = apply(modInfo) _

    val resolveExisting = (key: Key) => modInfo.get
      .contents
      .lookup(key)
      .flatMap(_.target)
      .point[F]

    val resolve = (key: Key) =>
      OptionT(resolveExisting(key))
        .orElse(OptionT(resolveMissing(key)))
        .run

    val resolved = targets
      .map(_.key)
      .traverse(k => resolve(k).map(Keyed(k, _)))

    resolved.map { targets.map(_.get) zip _ }
  }
}
