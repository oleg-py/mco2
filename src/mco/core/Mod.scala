package mco.core

import mco.core.paths._

trait Mod[F[_]] {
  val backingFile: Path

  final def label: String = backingFile.name.toString

  def list: F[Vector[RelPath]]
  def provide(content: Vector[RelPath]): fs2.Stream[F, Pointed[Path]]

  final def provideAll: fs2.Stream[F, Pointed[Path]] =
    fs2.Stream.eval(list).flatMap(provide)
}
