package mco.game.generic.store

import mco.Tests
import mco.core.paths._
import mco.stubs.cells._
import monix.eval.Coeval

class SimpleModTypesTest extends Tests.Sync {
  behavior of "SimpleModTypes"

  implicit val fs = immutableFs(
    seg"folder" -> dir(),
    seg"file" -> file(),
    seg"archive.7z" -> file()
  )

  val simpleModTypes = new SimpleModTypes[Coeval]

  it should "create mod for file" in {
    simpleModTypes(Path("file")).value should matchPattern {
      case Some(_) =>
    }
  }

  it should "create mod for folder" in {
    simpleModTypes(Path("folder")).value should matchPattern {
      case Some(_) =>
    }
  }

  it should "create mod for archive" in {
    simpleModTypes(Path("archive.7z")).value should matchPattern {
      case Some(_) =>
    }
  }

  it should "not create a mod for invalid name" in {
    simpleModTypes(Path("non-existing")).value shouldBe None
  }
}
