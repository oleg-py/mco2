package mco.core

import scalaz._
import std.vector._

import mco.util.syntax.fp._
import mco.core.state.ModState
import mco.data.{Key, Keyed, Path}


trait NameResolver[F[_]] {
  def apply(mod: Keyed[ModState], content: Key): F[Option[Path]]

  final def resolveAll(
    content: Content.Plain,
    modInfo: Keyed[(ModState, Mod[F])]
  )(implicit F: Monad[F]): Path.Temp[F, Vector[(Path, Option[Path])]] = { tmp =>
    val mod = modInfo.get._2
    val state = modInfo.map(_._1)
    val resolve = apply(state, _: Key)

    for {
      children <- mod.provideChildren(content).apply(tmp)
      resolved <- children.map(_.key).traverse(resolve)
    } yield children.map(_.get) zip resolved
  }
}
