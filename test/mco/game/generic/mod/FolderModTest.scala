package mco.game.generic.mod

import cats.Id
import mco.Tests
import mco.core.paths._
import mco.stubs.cells._
import monix.eval.Coeval


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

  val fm = new FolderMod[Coeval](path"mods/folderMod")

  "FolderMod#list" should "list all nested subfiles and subfolders" in {
    fm.list.value should contain only (
      rel"innerFile",
      rel"nonEmptySubdir/nestedFile",
      rel"nonEmptySubdir/nestedFile2"
    )
  }

  "FolderMod#provide" should "provide all requested children" in {
    val func = fm.provide(_: Vector[RelPath]).runLogSync.value

    func(Vector()) shouldBe empty

    val nested2 = "nonEmptySubdir/nestedFile2"

    func(Vector(rel"$nested2", rel"nonExisting")) should contain only
      Pointed(rel"$nested2", path"mods/folderMod/$nested2")

    val oks = Vector("innerFile", "emptySubdir", "nonEmptySubdir")
    func(oks.map(s => rel"$s")) should contain only
      Pointed(rel"innerFile", path"mods/folderMod/innerFile")
  }
}
