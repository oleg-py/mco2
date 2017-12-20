package mco.core.paths

import boopickle.Default._

/**
 * Trait with implementation of common functions of path-like elements
 */
object PathLike {
  trait Instance[A <: Instance[A]] { this: A =>
    val segments: Vector[Segment]
    def withSegments(segments: Vector[Segment]): A

    final def name: Segment = segments.lastOption.getOrElse(Segment.empty)
    final def extension: String = name.extension
    final def withExtension(ext: String) =
      withSegments(segments.init :+ segments.last.withExtension(ext))
  }

  trait Companion[A <: Instance[A]] {
    def apply(s: String): A = of(Segment.multi(s))
    def of(ss: Vector[Segment]): A
  }
}