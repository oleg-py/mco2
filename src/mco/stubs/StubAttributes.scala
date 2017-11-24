package mco.stubs

import mco.data.paths.{Path, SegTree}
import mco.stubs.cells._

import java.nio.file.attribute.{BasicFileAttributes, FileTime}


case class StubAttributes (path: Path, obj: Cell) extends BasicFileAttributes {
  private val stubTime = FileTime.fromMillis(1L)
  override def lastModifiedTime(): FileTime = stubTime
  override def creationTime(): FileTime = stubTime
  override def fileKey(): AnyRef = path
  override def isSymbolicLink: Boolean = false
  override def lastAccessTime(): FileTime = stubTime

  override def isRegularFile: Boolean = obj match {
    case SegTree.SegLeaf(_) => true
    case _ => false
  }

  override def size(): Long = obj match {
    case SegTree.SegLeaf(d) => d.length.toLong
    case _ => 1L
  }

  override def isOther: Boolean = false

  override def isDirectory: Boolean = obj match {
    case SegTree.SegRoot(_) => true
    case _ => false
  }
}
