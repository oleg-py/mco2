package mco.stubs

import scalaz._

sealed trait Cell

object Cell {
  case class Dir(contents: Map[String, Cell] = Map()) extends Cell
  case class File(data: ImmutableArray[Byte]) extends Cell
}
