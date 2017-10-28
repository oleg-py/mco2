package mco.io.state

import scalaz.Id._

import Hashing._
import better.files.File
import mco.Tests
import mco.data.Path
import mco.io.generic.{Filesystem, LocalFilesystem}
import mco.util.Capture

class MmapHashingTest extends Tests.SyncFixture with Tests.BetterFilesHelpers {
  implicit val hashing = new MmapHashing[Id] with LocalFilesystem[Id] with Capture.Impure

  // --------------------------------------------------------------------------

  behavior of "MmapHashing#hashFile on a file"

  it should "hash it contents" in { dir =>
    hashAt(dir / "test_folder/file1") shouldEqual helloHash
  }

  it should "hash independently of file location" in { dir =>
    val target = dir / "test_file"
    target.asFile.write("Hello")
    hashAt(target) shouldEqual helloHash
  }

  // --------------------------------------------------------------------------

  behavior of "MmapHashing#hashFile on a directory"

  it should "generate meaningful hash" in { dir =>
    hashAt(dir) shouldEqual testDirHash
  }

  // --------------------------------------------------------------------------

  val helloHash   = ( 753694413698530628L, 1860348619331993311L)
  val testDirHash = (3969179940749501790L, 1702180032275553555L)

  override type FixtureParam = Path
  override def withFixture(test: OneArgTest) = {
    for (tmp <- File.temporaryDirectory("mco-io-tests-hashing")) yield {
      File(getClass.getResource("/mco/io/algebras/fixture").toURI)
        .copyTo(tmp)
      withFixture(test.toNoArgTest(Path(tmp.pathAsString)))
    }
  }
}
