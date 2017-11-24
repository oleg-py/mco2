package mco.data

package object paths {
  implicit class PathInterpolations(sc: StringContext) {
    def path(cs: Any*) = Path(sc.s(cs: _*))
    def rel(cs: Any*) = RelPath(Segment.multi(sc.s(cs: _*)))
    def seg(cs: Any*) = Segment(sc.s(cs: _*))
  }
}
