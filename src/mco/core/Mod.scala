package mco.core

import scalaz._

import mco.data._
import mco.data.paths.{Path, RelPath}
import mco.util.syntax.fp._

trait Mod[F[_]] {
  val label: String
  def list: F[Vector[RelPath]]
  def provide(content: Vector[RelPath]): TempOp[F, Map[RelPath, Path]]
  final def provideVec(content: Vector[RelPath])(
    implicit F: Functor[F]
  ): TempOp[F, Vector[Keyed[Path]]] =
    provide(content).map(pathMap => content.map(lc => Keyed(lc, pathMap(lc))))

  // TODO: deprecate
  final def filterProvide(f: RelPath => Boolean)(
    implicit F: Functor[F]
  ): F[TempOp[F, Vector[Keyed[Path]]]] =
    for {
      children <- list
      vec = children.filter(f)
    } yield provideVec(vec)
}
