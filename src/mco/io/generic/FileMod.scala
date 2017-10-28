package mco.io.generic

import scalaz._
import syntax.applicative._

import mco.core._
import mco.data.{Key, Labelled, Path}


final class FileMod[F[_]: Filesystem: Applicative](path: Path)
  extends Mod[F]
{
  private val key = Key(path.name)

  def list: F[Vector[Labelled[Content]]] =
    Vector(Content.Component(key, path.name)).point[F].widen

  def provide(
    contents: Vector[Labelled[Content.Plain]]
  ): Path.Temp[F, Map[Key, Path]] = _ => {
    val result: Map[Key, Path] =
      if (contents.exists(_.key == key)) Map(key -> path)
      else Map()

    result.point[F]
  }
}
