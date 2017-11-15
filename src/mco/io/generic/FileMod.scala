package mco.io.generic

import scalaz._
import syntax.applicative._

import mco.core._
import mco.data.{Key, Keyed, Path}


final class FileMod[F[_]: Filesystem: Applicative](path: Path)
  extends Mod[F]
{
  private val key = Key(path.name)
  val label = path.name

  def list: F[Vector[Keyed[Content]]] =
    Vector(Content.Component(key)).point[F].widen

  def provide(
    contents: Vector[Key]
  ): Path.Temp[F, Map[Key, Path]] = _ => {
    val result: Map[Key, Path] =
      if (contents contains key) Map(key -> path)
      else Map()

    result.point[F]
  }
}
