package mco.core

import scalaz._

import mco.data._
import mco.data.paths.{Path, RelPath}
import mco.util.syntax.fp._

trait Mod[F[_]] {
  val label: String
  def list: F[Vector[Keyed[Content]]]
  def provide: TempOp[F, Vector[RelPath] => Map[RelPath, Path]]

  final def filterProvide(f: Keyed[Content] => Boolean)(
    implicit F: Monad[F]
  ): TempOp[F, Vector[Keyed[Path]]] = {
    val vec = TempOp(list.map(_ collect { case k if f(k) => k.key }))
    /*_*/
    val paths = vec <*> provide
    (vec |@| paths) { (files, pathMap) =>
      files.map(lc => Keyed(lc, pathMap(lc)))
    }
    /*_*/

  }

  final def provideChildren(c: Content)(
    implicit F: Monad[F]
  ): TempOp[F, Vector[Keyed[Path]]] = filterProvide(_.get == c)
}
