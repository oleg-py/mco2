package mco.core.state

import mco.Tests
import org.scalacheck.ScalacheckShapeless._

class DeltasProperties extends Tests.Props {
  property("Empty mod delta patch is noop") {
    forAll { (ms: ModState) =>
      Deltas.OfMod().patch(ms) shouldBe ms
    }
  }

  property("Enabled is copied to target mod") {
    forAll { (ms: ModState, enabled: Boolean) =>
      Deltas.OfMod(
        enabled = Some(enabled)
      ).patch(ms).stamp.enabled shouldBe enabled
    }
  }

  property("Empty content delta patch is noop") {
    forAll { (cs: ContentState) =>
      Deltas.OfContent(None).patch(cs) shouldBe cs
    }
  }

  property("Enabled is copied to target content") {
    forAll { (cs: ContentState, enabled: Boolean) =>
      Deltas.OfContent(enabled = Some(enabled))
        .patch(cs).stamp.enabled shouldBe enabled
    }
  }
}

