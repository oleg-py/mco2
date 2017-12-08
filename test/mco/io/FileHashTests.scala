package mco.io

import mco.Tests
import mco.core.paths._
import mco.stubs.cells._
import monix.eval.Coeval

class FileHashTests extends Tests.Spec {
  behavior of "FileHash#hashAt"

  implicit val fs: Filesystem[Coeval] = immutableFs(
    seg"dir" -> dir(
      seg"file1" -> file("Hello"),
      seg"file2" -> file("World"),
      seg"file3" -> file(", !")
    )
  )

  val computeHash = new FileHash[Coeval](_: Path).computed.value

  val helloHash =   ( 753694413698530628L, 1860348619331993311L)
  val testDirHash = ( 722433659508932188L, -772159419470578645L)

  it should "hash contents of a file" in {
    computeHash(path"dir/file1") shouldBe helloHash
  }

  it should "hash contents of a directory" in {
    computeHash(path"dir") shouldBe testDirHash
  }
}
