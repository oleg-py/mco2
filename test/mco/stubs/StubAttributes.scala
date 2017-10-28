package mco.stubs

import Cell._
import mco.data.Path

import java.nio.file.attribute.{BasicFileAttributes, FileTime}


case class StubAttributes(path: Path, obj: Cell) extends BasicFileAttributes {
  private val stubTime = FileTime.fromMillis(1L)
  override def lastModifiedTime(): FileTime = stubTime
  override def creationTime(): FileTime = stubTime
  override def fileKey(): AnyRef = path
  override def isSymbolicLink: Boolean = false
  override def lastAccessTime(): FileTime = stubTime

  override def isRegularFile: Boolean = obj match {
    case File(_) => true
    case _ => false
  }

  override def size(): Long = obj match {
    case File(d) => d.length.toLong
    case Dir(_) => 1L
  }

  override def isOther: Boolean = false

  override def isDirectory: Boolean = obj match {
    case Dir(_) => true
    case _ => false
  }
}
