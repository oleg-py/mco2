package mco.core

import scalaz._

import mco.data._
import mco.data.paths.{Path, RelPath}
import mco.util.syntax.fp._

trait Mod[F[_]] {
  val label: String
  def list: F[Vector[Keyed[Content]]]
  def provide(content: Vector[RelPath]): TempOp[F, Map[RelPath, Path]]

  final def filterProvide(f: Keyed[Content] => Boolean)(
    implicit F: Monad[F]
  ): F[TempOp[F, Vector[Keyed[Path]]]] =
    for {
      children <- list
      vec = children.filter(f).map(_.key)
    } yield provide(vec)
      .map(pathMap => vec.map(lc => Keyed(lc, pathMap(lc))))
}
