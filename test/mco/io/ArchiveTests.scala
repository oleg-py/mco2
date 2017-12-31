package mco.io

import mco.Tests
import mco.core.paths._
import mco.io.impls.LocalFilesystem
import monix.eval.Coeval


class ArchiveTests extends Tests.SpecFixture with Tests.TempDirsFixture {
  implicit val fs: Filesystem[Coeval] = new LocalFilesystem

  "Archive#entries" should "list archive entries" in { dirs =>
    for (ext <- Seq("7z", "rar", "zip")) {
      val arch = new Archive(dirs.src / seg"test_archive.$ext")

      arch.entries.runLogSync.value should contain only (
        rel"file1", rel"file2", rel"file3"
      )
    }
  }

  "Archive#extract" should "extract contents of rar archive" in { dirs =>
    val arch = new Archive(dirs.src / seg"test_archive.rar")
    val unpack = arch.extract(Map(
      rel"file1" -> dirs.target / seg"tg_1",
      rel"file3" -> dirs.target / seg"file3"
    ))

    unpack()

    (dirs.target / seg"tg_1").asFile.contentAsString shouldBe "Hello"
    (dirs.target / seg"file2") shouldNot exist
    (dirs.target / seg"file3").asFile.contentAsString shouldBe ", !"
  }
}
