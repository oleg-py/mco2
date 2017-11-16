package mco.stubs

import scalaz._

sealed trait Cell

object Cell {
  case class Dir(contents: Map[String, Cell] = Map()) extends Cell
  case class File(data: ImmutableArray[Byte]) extends Cell

  def dir(contents: (String, Cell)*): Dir = Dir(Map(contents: _*))
  def file(data: String = ""): File = File(ImmutableArray.fromArray(data.getBytes))
}
