package mco.core.paths

/**
 * Class representing a relative path
 *
 * Relative paths are used to indicate contents inside of a mod
 * Their normalization allows them to start with a double dot
 *
 * @param segments path segments
 */
case class RelPath(segments: Vector[Segment]) extends PathLike.Instance[RelPath] {
  override def toString: String = segments.mkString("/")

  override def withSegments(segments: Vector[Segment]): RelPath =
    RelPath(segments.mkString("/"))
}

object RelPath extends PathLike.Companion[RelPath] {
  override def of(ss: Vector[Segment]): RelPath = new RelPath(ss) { }
}

