package mco.io.impls

import cats.data.StateT
import mco.Tests
import mco.core.paths._
import mco.core.vars.JavaSerializableVar
import mco.io.Filesystem
import mco.io.impls.FilesystemVarStamping.FileStamp
import mco.stubs.StateTVar
import mco.stubs.cells._
import monix.eval.Coeval
import monocle.function.fields

import java.nio.ByteBuffer


//noinspection TypeAnnotation
class FilesystemVarStampingSpec extends Tests.Spec {
  private val state = new StateTVar[Coeval, (Cell, Map[InnerPath, FileStamp])]
  implicit val fs = statefulFs(state.zoom(fields.first))
  val stamping = new FilesystemVarStamping(state.zoom(fields.second))
  val filePath = path"unknown"
  val innerPath = (filePath, rel"")

  behavior of "FilesystemVarStamping#likelySame"

  val compare = stamping.likelySame(innerPath, filePath, filePath)
  val mkFile = fs.writeFile(filePath, ByteBuffer.wrap("Hello".getBytes))

  it should "return false for unfamiliar files" in {
    val m = for {
      _ <- mkFile
      r <- compare
    } yield r

    m.runA((dir(), Map())).value shouldBe false
  }

  it should "return false for nonexisting files" in {
    val m = for {
      r <- compare
    } yield r

    m.runA((dir(), Map())).value shouldBe false
  }

  it should "return true directly after overwrite" in {
    val m = for {
      _ <- mkFile
      _ <- stamping.overwrite(innerPath, filePath)
      r <- compare
    } yield r

    m.runA((dir(), Map())).value shouldBe true
  }
}
