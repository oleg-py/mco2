package mco.io.generic

import scalaz.Id._

import mco.Tests
import mco.core.Content
import mco.data.paths._
import mco.data.Keyed
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

  val fm = new FolderMod[Id](path"mods/folderMod")

  "FolderMod#label" should "use folder name as label" in {
    fm.label shouldBe "folderMod"
  }

  "FolderMod#list" should "list all nested subfiles and subfolders" in {
    (fm.list: Vector[Keyed[Content]]) should contain theSameElementsAs Vector(
      Content.Component(rel"innerFile"),
      Content.Component(rel"nonEmptySubdir/nestedFile"),
      Content.Component(rel"nonEmptySubdir/nestedFile2"),
      Keyed(rel"emptySubdir", Content.Container(Vector())),
      Keyed(rel"nonEmptySubdir", Content.Container(Vector(
        Content.Component(rel"nonEmptySubdir/nestedFile"),
        Content.Component(rel"nonEmptySubdir/nestedFile2")
      )))
    )
  }

  "FolderMod#provide" should "provide all requested children" in {
    /*_*/
    val func: (Vector[RelPath] => Map[RelPath, Path]) = fm.provide.runFS
    /*_*/

    func(Vector()) shouldBe Map()
    val nested2 = "nonEmptySubdir/nestedFile2"
    func(Vector(rel"$nested2", rel"nonExisting")) shouldBe Map(
      rel"$nested2" -> path"mods/folderMod/$nested2"
    )

    val oks = Vector("innerFile", "emptySubdir", "nonEmptySubdir")
    func(oks.map(s => rel"$s")) shouldBe oks
      .map(s => rel"$s" -> path"mods/folderMod/$s")
      .toMap
  }
}
