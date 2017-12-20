package mco.stubs

import mco.core.paths._


// TODO - remove / inline / define better utils
object cells {
  type Cell = SegTree[Array[Byte]]
  val Dir = SegTree.SegRoot
  val File = SegTree.SegLeaf
  def dir(contents: (Segment, Cell)*): Cell = SegTree.root(Map(contents: _*))
  def file(data: String = ""): Cell = SegTree.leaf(data.getBytes)
}
