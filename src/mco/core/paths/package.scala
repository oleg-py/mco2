package mco.core

import boopickle.Default._

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

  /**
   * Class representing value contained with a relative path
   *
   * @tparam A type of value contained
   */
  type Pointed[+A] = (RelPath, A)

  implicit val segmentPicker: Pickler[Segment] =
    transformPickler[Segment, String](Segment(_))(_.toString)

  implicit val relPickler =
    transformPickler[RelPath, Vector[Segment]](RelPath.of)(_.segments)

  implicit val pathPickler =
    transformPickler[Path, Vector[Segment]](Path.of)(_.segments)
}
