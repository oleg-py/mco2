package mco.core.paths

trait PathLike[A <: PathLike[A]] { this: A =>
  val segments: Vector[Segment]
  def name: Segment = segments.lastOption.getOrElse(Segment.empty)
  def extension: String = name.extension
}
