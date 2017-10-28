package mco.core

import scalaz._
import std.vector._

import mco.util.syntax.fp._
import mco.core.state.ModState
import mco.data.{Labelled, Path}


trait NameResolver[F[_], C <: Content.Plain] {
  def apply(mod: Labelled[ModState], content: Labelled[C]): F[Option[Path]]

  final def resolveAll(
    content: C,
    modInfo: Labelled[(ModState, Mod[F])]
  )(implicit F: Monad[F]): Path.Temp[F, Vector[(Path, Option[Path])]] = tmp => {
    val mod = modInfo.get._2
    val state = modInfo.map(_._1)
    val resolve = apply(state, _: Labelled[C])

    for {
      children <- mod.provideChildren(content).apply(tmp)
      resolved <- children.map(_.map(_._1)).traverse(resolve)
    } yield children.map(_.get._2) zip resolved
  }
}
