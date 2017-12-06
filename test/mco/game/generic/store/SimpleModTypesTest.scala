package mco.game.generic.store

import scalaz.Id._

import mco.Tests
import mco.core.Capture.yolo._
import mco.core.paths._
import mco.io.impls.SlowArchiving
import mco.stubs.cells._

class SimpleModTypesTest extends Tests.Sync {
  behavior of "SimpleModTypes"

  implicit val fs = immutableFs(
    seg"folder" -> dir(),
    seg"file" -> file()
  )
  implicit val archiving = new SlowArchiving[Id]

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
