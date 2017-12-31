package mco.game.generic.extractors

import mco.Tests
import mco.core.paths._
import mco.stubs.cells._
import monix.eval.Coeval


class FolderExtractorTest extends Tests.Spec {
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

  val fm = new FolderExtractor[Coeval](path"mods/folderMod")

  "FolderExtractor#entries" should "list children deeply" in {
    fm.entries.runLogSync.value should contain only (
      rel"innerFile",
      rel"nonEmptySubdir/nestedFile",
      rel"nonEmptySubdir/nestedFile2"
    )
  }

  "FolderMod#provide" should "provide requested children from FS" in {
    val func = fm.provide(_: Set[RelPath]).runLogSync.value

    func(Set()) shouldBe empty

    val nested2 = "nonEmptySubdir/nestedFile2"

    func(Set(rel"$nested2", rel"nonExisting")) should contain only
      (rel"$nested2" -> path"mods/folderMod/$nested2")

    val oks = Set("innerFile", "emptySubdir", "nonEmptySubdir")
    func(oks.map(s => rel"$s")) should contain only
      (rel"innerFile" -> path"mods/folderMod/innerFile")
  }
}
