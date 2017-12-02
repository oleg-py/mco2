package mco.io.generic

import scalaz.Id._

import mco.Tests
import mco.core.Content
import mco.data.paths._
import mco.data.Keyed
import mco.stubs.cells._


class FolderModTest extends Tests.Sync {
  implicit val filesystem = immutableFs(
    seg"mods" -> dir(
      seg"folderMod" -> dir(
        seg"innerFile" -> file("InnerFileContent"),
        seg"emptySubdir" -> dir(),
        seg"nonEmptySubdir" -> dir(
          seg"nestedFile" -> file("NestedFileContent"),
          seg"nestedFile2" -> file("NestedFile2Content")
        )
      )
    )
  )

  val fm = new FolderMod[Id](path"mods/folderMod")

  "FolderMod#label" should "use folder name as label" in {
    fm.label shouldBe "folderMod"
  }

  "FolderMod#list" should "list all nested subfiles and subfolders" in {
    (fm.list: Vector[RelPath]) should contain theSameElementsAs Vector(
      rel"innerFile",
      rel"nonEmptySubdir/nestedFile",
      rel"nonEmptySubdir/nestedFile2"
    )
  }

  "FolderMod#provide" should "provide all requested children" in {
    val func = fm.provide(_: Vector[RelPath]).runFS

    func(Vector()) shouldBe Map()
    val nested2 = "nonEmptySubdir/nestedFile2"
    func(Vector(rel"$nested2", rel"nonExisting")) shouldBe Map(
      rel"$nested2" -> path"mods/folderMod/$nested2"
    )

    val oks = Vector("innerFile", "emptySubdir", "nonEmptySubdir")
    func(oks.map(s => rel"$s")) shouldBe Map(
      rel"innerFile" -> path"mods/folderMod/innerFile"
    )
  }
}
