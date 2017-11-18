package mco.io.generic

import scalaz._
import syntax.applicative._

import mco.core._
import mco.data._


final class FileMod[F[_]: Filesystem: Applicative](path: Path)
  extends Mod[F]
{
  private val key = Key(path.name)
  val label = path.name

  def list: F[Vector[Keyed[Content]]] =
    Vector(Content.Component(key)).point[F].widen

  def provide = TempOp {
      val fn = (c: Vector[Key]) =>
        if (c contains key) Map(key -> path)
        else Map.empty[Key, Path]

    fn.point[F]
  }
}
