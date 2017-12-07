package mco.core

import cats._
import cats.syntax.functor._

import mco.core.paths._
import mco.io.InTemp

trait Mod[F[_]] {
  val backingFile: Path

  final val label: String = backingFile.name.toString

  def list: F[Vector[RelPath]]
  def provide(content: Vector[RelPath]): InTemp[F, Map[RelPath, Path]]
  final def provideVec(content: Vector[RelPath])(
    implicit F: Functor[F]
  ): InTemp[F, Vector[Pointed[Path]]] =
    provide(content).map(pathMap => content.map(lc => paths.Pointed(lc, pathMap(lc))))

  // TODO: deprecate
  final def filterProvide(f: RelPath => Boolean)(
    implicit F: Functor[F]
  ): F[InTemp[F, Vector[Pointed[Path]]]] =
    for {
      children <- list
      vec = children.filter(f)
    } yield provideVec(vec)
}
