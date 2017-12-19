package mco.core.state

import mco.Tests
import mco.core.Status
import org.scalacheck.ScalacheckShapeless._

class DeltasProperties extends Tests.Props {
  property("Empty mod delta patch is noop") {
    forAll { (ms: ModState) =>
      Deltas.OfMod().patch(ms) shouldBe ms
    }
  }

  property("Status is copied to target mod") {
    forAll { (ms: ModState, status: Status) =>
      Deltas.OfMod(
        status = Some(status)
      ).patch(ms).status shouldBe status
    }
  }

  property("Empty content delta patch is noop") {
    forAll { (cs: ContentState) =>
      Deltas.OfContent(None).patch(cs) shouldBe cs
    }
  }

  property("Enabled is copied to target content") {
    forAll { (cs: ContentState, status: Status) =>
      Deltas.OfContent(status = Some(status))
        .patch(cs).status shouldBe status
    }
  }
}

