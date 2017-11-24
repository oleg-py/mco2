package mco.data.paths

case class RelPath(segments: Vector[Segment]) {
  override def toString = segments.mkString("/")
}

object RelPath {
  def apply(s: String): RelPath = RelPath(Segment.multi(Seq(s)))
}

