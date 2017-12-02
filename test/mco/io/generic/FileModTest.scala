package mco.io.generic

import scalaz.Id._

import mco.Tests
import mco.core.Content
import mco.data.Keyed
import mco.data.paths._
import mco.stubs.cells._
import org.scalatest.LoneElement._

class FileModTest extends Tests.Sync {
  implicit val filesystem = immutableFs(
    seg"mods" -> dir(
      seg"fileMod.dat" -> file("Hello")
    )
  )
  val fm = new FileMod[Id](path"mods/fileMod.dat")

  "FileMod#label" should "use filename as label" in {
    fm.label shouldBe "fileMod.dat"
  }

  "FileMod#list" should "provide file itself as a single component" in {
    (fm.list: Vector[RelPath])
      .loneElement shouldBe rel"fileMod.dat"
  }

  /*_*/
  "FileMod#provide" should "provide only own file" in {
    val fun = fm.provide(_: Vector[RelPath]).runFS
    fun(Vector(rel"fileMod.dat")) shouldBe Map(
      rel"fileMod.dat" -> path"mods/fileMod.dat"
    )
    fun(Vector(rel"zuul.qux")) shouldBe Map()
  }
}
