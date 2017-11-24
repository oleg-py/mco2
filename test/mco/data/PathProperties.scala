package mco.data

import mco.Tests
import mco.data.paths._

class PathProperties extends Tests.Prop {
  property("self inverse") {
    forAll { (p: Path) =>
      p.fromTo(p) should be (Vector())
    }
  }

  property("relTo -> `/` inversion") {
    forAll { (p1: Path, p2: Path) =>
      p1 / (p2 fromToP p1) should be (p2)
    }
  }

//  property("`/` -> relTo normalized inversion") {
//    forAll { (p: Path, ss: Vector[Segment]) =>
//      (p / RelPath(ss)).relToS(p) should be (Path.normalize(ss))
//    }
//  }

  property("`/s/..` identity for valid segments") {
    forAll { (p: Path, s: Segment) =>
      whenever(s != Segment.`..`) {
        (p / s / Segment.`..`) should be (p)
      }
    }
  }

  property("`/../name` identity") {
    forAll { (p: Path) =>
      (p / Segment.`..` / p.name) should be (p)
    }
  }

  property("of(segments) identity") {
    forAll { (p: Path) =>
      Path.of(p.segments) should be (p)
    }
  }

  property("toString identity") {
    forAll { (p: Path) =>
      Path(p.toString) should be (p)
    }
  }

}
