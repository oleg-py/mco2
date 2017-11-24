package mco.stubs

import scalaz._

sealed trait Cell

// TODO - deprecate
object Cell {
  case class Dir(contents: Map[String, Cell] = Map()) extends Cell
  case class File(data: Array[Byte]) extends Cell

  def dir(contents: (String, Cell)*): Dir = Dir(Map(contents: _*))
  def file(data: String = ""): File = File(data.getBytes)
}
