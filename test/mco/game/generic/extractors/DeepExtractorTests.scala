package mco.game.generic.extractors

import mco.Tests
import mco.core.paths._
import mco.stubs.cells.{dir, file}
import monix.eval.Coeval


class DeepExtractorTests extends Tests.Spec {
  implicit val filesystem = immutableFs(
    seg"mods" -> dir(
      seg"folderMod" -> dir(
        seg"innerFile" -> file("InnerFileContent"),
        seg"emptySubdir" -> dir(),
        seg"nonEmptySubdir" -> dir(
          seg"nestedFile" -> file("NestedFileContent"),
        )
      )
    ),
    seg"file" -> file()
  )


  "Extractor.deep entries" should "use provide of given factory" in {
    val ex = Extractor.deep(FolderExtractor[Coeval])
    ex(path"mods").entries.runLogSync.value should contain only (
      rel"folderMod/innerFile",
      rel"folderMod/nonEmptySubdir/nestedFile"
    )
  }

  "Extractor.deep entries" should "provide a singleton path if no factories applicable" in {
    val ex = Extractor.deep[Coeval]()
    ex(path"file").entries.runLogSync.value should contain only rel"file"
  }

}
