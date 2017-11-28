package mco.core

import scalaz._

import mco.data._
import mco.data.paths.{Path, RelPath}
import mco.util.syntax.fp._

trait Mod[F[_]] {
  val label: String
  def list: F[Vector[RelPath]]
  def provide(content: Vector[RelPath]): TempOp[F, Map[RelPath, Path]]

  final def filterProvide(f: RelPath => Boolean)(
    implicit F: Functor[F]
  ): F[TempOp[F, Vector[Keyed[Path]]]] =
    for {
      children <- list
      vec = children.filter(f)
    } yield provide(vec)
      .map(pathMap => vec.map(lc => Keyed(lc, pathMap(lc))))
}
