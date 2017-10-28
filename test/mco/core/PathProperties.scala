package mco.core

import mco.Tests
import org.scalacheck.Arbitrary
import Arbitrary.{arbitrary => arb}
import mco.data.Path

class PathProperties extends Tests.Prop {
  property("self inverse") {
    forAll { (p: Path) =>
      p.relTo(p) should be (Vector())
    }
  }

  property("relTo -> `/` inversion") {
    forAll { (p1: Path, p2: Path) =>
      p1 / (p2 relTo p1) should be (p2)
    }
  }

  property("`/` -> relTo normalized inversion") {
    forAll { (p: Path, ss: Seq[String]) =>
      (p / ss).relTo(p) should be (Path.normalize(ss))
    }
  }

  property("`/s/..` identity for valid segments") {
    forAll { (p: Path, s: String) =>
      whenever(!s.contains("..") && Path.normalize(Seq(s)).length == 1) {
        (p / s / "..") should be (p)
      }
    }
  }

  property("`/../name` identity") {
    forAll { (p: Path) =>
      (p / ".." / p.name) should be (p)
    }
  }

  property("of(segments) identity") {
    forAll { (p: Path) =>
      Path.of(p.segments) should be (p)
    }
  }

  property("normalized asString <-> segments equiv") {
    forAll { (p: Path) =>
      Path.normalize(Seq(p.asString)) should be (p.segments)
    }
  }

  implicit val arbitrary: Arbitrary[Path] =
    Arbitrary(arb[List[String]].map(Path.of))

}
