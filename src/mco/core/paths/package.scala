package mco.core

package object paths {

  /**
   * String interpolators for segments, absolute and relative paths
   */
  implicit class PathInterpolations(sc: StringContext) {
    def path(cs: Any*) = Path(sc.s(cs: _*))
    def rel(cs: Any*) = RelPath(Segment.multi(sc.s(cs: _*)))
    def seg(cs: Any*) = Segment(sc.s(cs: _*))
  }

  /**
   * Type representing an object (RelPath) within another object (Path).
   * Useful for storing metadata of files that might not be always
   * available at the same location
   */
  type InnerPath = (Path, RelPath)
}
