package mco.data.paths

import scalaz._
import std.vector._

case class RelPath(segments: Vector[Segment]) {
  // TODO move to common supertrait
  def name: Segment = segments.lastOption.getOrElse(Segment.empty)
  def extension: String = name.extension

  override def toString = segments.mkString("/")
}

object RelPath {
  def apply(s: String): RelPath = RelPath(Segment.multi(Seq(s)))
  implicit val relPathOrder: Order[RelPath] = Order.orderBy(_.segments)
}

