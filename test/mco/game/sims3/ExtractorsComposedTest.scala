package mco.game.sims3

import better.files._
import mco.Tests
import mco.core.paths._
import mco.game.generic.extractors.{ArchiveExtractor, Extractor}
import mco.game.sims3.extractors.Sims3PackExtractor
import mco.io.{Archiving, Filesystem}
import mco.io.impls.{LocalFilesystem, SevenZipArchiving}
import monix.eval.Coeval

class ExtractorsComposedTest extends Tests.SpecFixture with Tests.TempDirsFixture {
  implicit val fs: Filesystem[Coeval] = new LocalFilesystem[Coeval]
  implicit val arch: Archiving[Coeval] = new SevenZipArchiving[Coeval]

  val cwd = Path(File(".").pathAsString)
  implicit val s3ce: S3CE[Coeval] = new S3CE[Coeval](cwd / seg"s3ce.exe")


  "deep archive" should "provide data from sims3pack in zips" in { dirs =>
    val ex = Extractor.deep(ArchiveExtractor[Coeval], Sims3PackExtractor[Coeval])
    val prov = ex(dirs.src / seg"sims3" / seg"Basic Geometrics_marcorse.zip")

    val names = Vector(
      "Bead Curtain",
      "Frames",
      "Tic Tac Toe",
      "AlwaysDiamonds",
      "Circle&Squares")
      .map(_ ++ "_marcorse")
      .map(name => rel"$name.Sims3Pack/$name.package")

    prov.entries.runLogSync.value should contain theSameElementsAs names
  }
}
