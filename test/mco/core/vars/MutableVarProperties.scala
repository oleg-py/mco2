package mco.core.vars

import mco.Tests
import monix.eval.Coeval


class MutableVarProperties extends Tests.Props {
  def mkVar(init: String): Var[Coeval, String] = MutableVar[Coeval, String](init).value

  property("write >> read consistency") {
    forAll { (s: String) =>
      val x = mkVar("")
      val out = for {
        _  <- x := s
        ss <- x()
      } yield ss
      out.value shouldBe s
    }
  }

  property("transform >> read consistency") {
    forAll { (s: String, f: String => String) =>
      val x = mkVar(s)
      val out = for {
        _  <- x ~= f
        ss <- x()
      } yield ss
      out.value shouldBe f(s)
    }
  }
}
