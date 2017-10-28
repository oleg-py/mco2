package mco.io

import scalaz._
import std.vector._

import mco.io.generic.Filesystem
import Filesystem._
import mco.data.Path
import mco.util.syntax.fp._

package object generic {
  def installResolved[F[_]: Applicative: Filesystem](
    targets: Vector[(Path, Option[Path])],
    targetFilter: Path => Boolean
  ) = targets traverse_ {
    case (from, Some(to)) if targetFilter(to) => copy(from, to)
    case _ => ().point[F]
  }
}
