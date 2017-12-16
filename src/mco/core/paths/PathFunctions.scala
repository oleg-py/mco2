package mco.core.paths

/**
 * Trait with implementation of common functions of path-like elements
 *
 * @tparam A self-type to limit the paths at
 */
trait PathFunctions[A <: PathFunctions[A]] { this: A =>
  val segments: Vector[Segment]
  def withSegments(segments: Vector[Segment]): A

  final def name: Segment = segments.lastOption.getOrElse(Segment.empty)
  final def extension: String = name.extension
  final def withExtension(ext: String) =
    withSegments(segments.init :+ segments.last.withExtension(ext))
}
