package mco.core

import scalaz._

import mco.data.{Key, Labelled, Path}
import mco.util.syntax.fp._


trait Mod[F[_]] {
  def list: F[Vector[Labelled[Content]]]
  def provide(contents: Vector[Labelled[Content.Plain]]): Path.Temp[F, Map[Key, Path]]

  final def plainChildren[C <: Content.Plain](c: C)(implicit F: Functor[F]): F[Vector[Labelled[C]]] =
    list map(vec => vec collect { case Labelled(k, l, `c`) => c(k, l) })

  final def provideChildren[C <: Content.Plain](c: C)(
    implicit F: Monad[F]
  ): Path.Temp[F, Vector[Labelled[(C, Path)]]] = tempF =>
    for {
      files <- plainChildren(c)
      paths <- provide(files)(tempF)
    } yield for (lc <- files) yield lc.coflatMap(x => (x.get, paths(x.key)))
}
