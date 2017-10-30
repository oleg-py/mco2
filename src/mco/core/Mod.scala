package mco.core

import scalaz._

import mco.data.{Key, Keyed, Path}
import mco.util.syntax.fp._


trait Mod[F[_]] {
  def list: F[Vector[Keyed[Content]]]
  def provide(contents: Vector[Key]): Path.Temp[F, Map[Key, Path]]

  final def plainChildren(c: Content.Plain)(implicit F: Functor[F]): F[Vector[Key]] =
    list map(vec => vec collect { case Keyed(k, `c`) => k })

  final def provideChildren(c: Content.Plain)(
    implicit F: Monad[F]
  ): Path.Temp[F, Vector[Keyed[Path]]] = tempF =>
    for {
      files <- plainChildren(c)
      paths <- provide(files)(tempF)
    } yield for (lc <- files) yield Keyed(lc, paths(lc))
}
