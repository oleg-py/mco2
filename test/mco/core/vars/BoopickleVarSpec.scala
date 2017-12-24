package mco.core.vars

import boopickle.Default._
import mco.Tests
import mco.core.paths._
import mco.io.Filesystem
import mco.stubs.StateTVar
import mco.stubs.cells._


class BoopickleVarSpec extends Tests.Props {
  implicit val fs: Filesystem[FState] = statefulFs(new StateTVar)
  val variable = new BoopickleVar[FState, String](path"var.bin")

  property("write >> read consistency") {
    forAll { (s: String) =>
      val computation = for {
        _  <- variable := s
        ss <- variable()
      } yield ss
      computation.runA(dir()).value shouldBe s
    }
  }

  property("write >> transform >> read consistency") {
    forAll { (s: String, f: String => String) =>
      val computation = for {
        _  <- variable := s
        _  <- variable ~= f
        ss <- variable()
      } yield ss
      computation.runA(dir()).value shouldBe f(s)
    }
  }
}
