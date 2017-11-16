package mco.io.generic

import mco.Tests
import mco.data.Path
import mco.stubs.Cell._
import mco.stubs.{ImmutableVar, VarInterpreter}
import scalaz.Id._
import scalaz.syntax.id._

class SimpleModTypesTest extends Tests.Sync {
  behavior of "SimpleModTypes"

  val root = dir(
    "folder" -> dir(),
    "file" -> file()
  ).|>(new ImmutableVar(_))

  implicit val fsAlgebra = new VarInterpreter[Id](root)
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
