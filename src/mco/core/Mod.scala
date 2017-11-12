package mco.core

import scalaz._

import mco.data.{Key, Keyed, Path}
import mco.util.syntax.fp._


trait Mod[F[_]] {
  def list: F[Vector[Keyed[Content]]]
  def provide(contents: Vector[Key]): Path.Temp[F, Map[Key, Path]]

  final def filterProvide(f: Keyed[Content] => Boolean)(
    implicit F: Monad[F]
  ): Path.Temp[F, Vector[Keyed[Path]]] = tempF =>
    for {
      children <- list
      files = children collect { case k if f(k) => k.key }
      paths <- provide(files)(tempF)
    } yield for (lc <- files) yield Keyed(lc, paths(lc))

  final def provideChildren(c: Content.Plain)(
    implicit F: Monad[F]
  ): Path.Temp[F, Vector[Keyed[Path]]] = filterProvide(_.get == c)
}
