package mco.io.generic

import mco.Tests
import mco.stubs.Cell._
import mco.stubs.{ImmutableVar, VarFilesystem}
import scalaz.Id._
import scalaz.syntax.id._

import mco.data.paths.Path

class SimpleModTypesTest extends Tests.Sync {
  behavior of "SimpleModTypes"

  implicit val fs = immutableFs(
    "folder" -> dir(),
    "file" -> file()
  )
  val simpleModTypes = new SimpleModTypes[Id]

  it should "create mod for file" in {
    simpleModTypes(Path("file")) should matchPattern {
      case Some(_) =>
    }
  }

  it should "create mod for folder" in {
    simpleModTypes(Path("folder")) should matchPattern {
      case Some(_) =>
    }
  }

  it should "not create a mod for invalid name" in {
    simpleModTypes(Path("non-existing")) shouldBe None
  }
}
