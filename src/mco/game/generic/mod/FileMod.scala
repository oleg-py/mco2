package mco.game.generic.mod

import scalaz._
import scalaz.syntax.applicative._

import mco.core._
import mco.core.paths._
import mco.io.{InTemp, Filesystem}


final class FileMod[F[_]: Filesystem: Applicative](
  file: Path
)
  extends Mod[F]
{
  val label = file.name.toString

  def list: F[Vector[RelPath]] =
    Vector(rel"${file.name}").point[F]

  def provide(content: Vector[RelPath]) = InTemp {
    val result =
      if (content contains rel"${file.name}") Map(rel"${file.name}" -> file)
      else Map.empty[RelPath, Path]
    result.point[F]
  }
}
