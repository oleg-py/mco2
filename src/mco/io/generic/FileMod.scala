package mco.io.generic

import scalaz._
import syntax.applicative._

import mco.core._
import mco.data._
import mco.data.paths._


final class FileMod[F[_]: Filesystem: Applicative](
  file: Path
)
  extends Mod[F]
{
  val label = file.name.toString

  def list: F[Vector[Keyed[Content]]] =
    Vector(Content.Component(rel"${file.name}")).point[F]

  def provide(content: Vector[RelPath]) = TempOp {
    val result =
      if (content contains rel"${file.name}") Map(rel"${file.name}" -> file)
      else Map.empty[RelPath, Path]
    result.point[F]
  }
}
