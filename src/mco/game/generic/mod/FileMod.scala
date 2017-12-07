package mco.game.generic.mod

import scalaz._
import scalaz.syntax.applicative._

import mco.core._
import mco.core.paths._
import mco.io.{InTemp, Filesystem}


final class FileMod[F[_]: Filesystem: Applicative](
  override val backingFile: Path
) extends Mod[F]
{
  def list: F[Vector[RelPath]] =
    Vector(rel"${backingFile.name}").point[F]

  def provide(content: Vector[RelPath]) = InTemp {
    val result =
      if (content contains rel"${backingFile.name}") {
        Map(rel"${backingFile.name}" -> backingFile)
      } else {
        Map.empty[RelPath, Path]
      }
    result.point[F]
  }
}
