package mco.core.paths


case class RelPath(segments: Vector[Segment]) extends PathLike[RelPath] {
  override def toString = segments.mkString("/")
}

object RelPath {
  def apply(s: String): RelPath = RelPath(Segment.multi(s))
}

