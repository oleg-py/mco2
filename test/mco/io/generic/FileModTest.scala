package mco.io.generic

import scalaz.Id._

import mco.Tests
import mco.core.Content
import mco.data.{Key, Keyed, Path}
import mco.stubs.Cell._
import org.scalatest.LoneElement._

class FileModTest extends Tests.Sync {
  implicit val filesystem = immutableFs(
    "mods" -> dir(
      "fileMod.dat" -> file("Hello")
    )
  )
  val fm = new FileMod[Id](p"mods/fileMod.dat")

  "FileMod#label" should "use filename as label" in {
    fm.label shouldBe "fileMod.dat"
  }

  "FileMod#list" should "provide file itself as a single component" in {
    (fm.list: Vector[Keyed[Content]])
      .loneElement shouldBe Keyed(key"fileMod.dat", Content.Component)
  }

  /*_*/
  "FileMod#provide" should "provide only own file" in {
    val fun = fm.provide.runFS
    fun(Vector(key"fileMod.dat")) shouldBe Map(
      key"fileMod.dat" -> p"mods/fileMod.dat"
    )
    fun(Vector(key"zuul.qux")) shouldBe Map()
  }
}
