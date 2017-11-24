package mco.data.paths

import scalaz._
import std.vector._

case class RelPath(segments: Vector[Segment]) extends PathLike[RelPath] {
  override def toString = segments.mkString("/")
}

object RelPath {
  def apply(s: String): RelPath = RelPath(Segment.multi(s))
  implicit val relPathOrder: Order[RelPath] = Order.orderBy(_.segments)
}

