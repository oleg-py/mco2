package mco.core.paths

/**
 * Trait with implementation of common functions of path-like elements
 *
 * @tparam A self-type to limit the paths at
 */
trait PathLike[A <: PathLike[A]] { this: A =>
  val segments: Vector[Segment]
  def name: Segment = segments.lastOption.getOrElse(Segment.empty)
  def extension: String = name.extension
}
