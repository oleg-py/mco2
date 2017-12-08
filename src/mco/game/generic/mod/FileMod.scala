package mco.game.generic.mod

import cats._
import cats.syntax.applicative._

import mco.core._
import mco.core.paths._
import mco.io.Filesystem


final class FileMod[F[_]: Filesystem: Applicative](
  override val backingFile: Path
) extends Mod[F]
{
  private val contentPath = rel"${backingFile.name}"

  def list: F[Vector[RelPath]] =
    Vector(contentPath).pure[F]

  override def provide(content: Vector[RelPath]): fs2.Stream[F, Pointed[Path]] = {
    val seq = content.filter(_ == contentPath).map(Pointed(_, backingFile))
    fs2.Stream.emits(seq).covary
  }
}
