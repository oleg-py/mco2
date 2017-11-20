package mco.io.generic

import scalaz.Id._

import mco.Tests
import mco.core.Content
import mco.data.{Key, Keyed, Path}
import mco.stubs.Cell._


class FolderModTest extends Tests.Sync {
  implicit val filesystem = immutableFs(
    "mods" -> dir(
      "folderMod" -> dir(
        "innerFile" -> file("InnerFileContent"),
        "emptySubdir" -> dir(),
        "nonEmptySubdir" -> dir(
          "nestedFile" -> file("NestedFileContent"),
          "nestedFile2" -> file("NestedFile2Content")
        )
      )
    )
  )

  val fm = new FolderMod[Id](p"mods/folderMod")

  "FolderMod#label" should "use folder name as label" in {
    fm.label shouldBe "folderMod"
  }

  "FolderMod#list" should "list all nested subfiles and subfolders" in {
    (fm.list: Vector[Keyed[Content]]) should contain theSameElementsAs Vector(
      Content.Component(key"innerFile"),
      Content.Component(key"nonEmptySubdir/nestedFile"),
      Content.Component(key"nonEmptySubdir/nestedFile2"),
      Keyed(key"emptySubdir", Content.Container(Vector())),
      Keyed(key"nonEmptySubdir", Content.Container(Vector(
        Content.Component(key"nonEmptySubdir/nestedFile"),
        Content.Component(key"nonEmptySubdir/nestedFile2")
      )))
    )
  }

  "FolderMod#provide" should "provide all requested children" in {
    /*_*/
    val func: (Vector[Key] => Map[Key, Path]) = fm.provide.runFS
    /*_*/

    func(Vector()) shouldBe Map()
    val nested2 = "nonEmptySubdir/nestedFile2"
    func(Vector(key"$nested2", key"nonExisting")) shouldBe Map(
      key"$nested2" -> p"mods/folderMod/$nested2"
    )

    val oks = Vector("innerFile", "emptySubdir", "nonEmptySubdir")
    func(oks.map(s => key"$s")) shouldBe oks
      .map(s => key"$s" -> p"mods/folderMod/$s")
      .toMap
  }
}
