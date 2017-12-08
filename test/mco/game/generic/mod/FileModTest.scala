package mco.game.generic.mod

import cats.Id
import mco.Tests
import mco.core.paths._
import mco.stubs.cells._
import monix.eval.Coeval
import org.scalatest.LoneElement._

class FileModTest extends Tests.Spec {
  implicit val filesystem = immutableFs(
    seg"mods" -> dir(
      seg"fileMod.dat" -> file("Hello")
    )
  )
  val fm = new FileMod[Coeval](path"mods/fileMod.dat")

  "FileMod#list" should "provide file itself as a single component" in {
    fm.list.value should contain only rel"fileMod.dat"
  }

  /*_*/
  "FileMod#provide" should "provide only own file" in {
    val fun = fm.provide(_: Vector[RelPath]).runLogSync.value
    fun(Vector(rel"fileMod.dat")) should contain only
      Pointed(rel"fileMod.dat", path"mods/fileMod.dat")

    fun(Vector(rel"zuul.qux")) shouldBe empty
  }
}
