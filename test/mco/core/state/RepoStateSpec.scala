package mco.core.state

import mco.Tests
import mco.core.{ContentKind, Status}
import mco.core.paths._


class RepoStateSpec extends Tests.Spec {
  private def modState(rel: RelPath, c: (RelPath, ContentState)*): Pointed[ModState] = {
    Pointed(rel, ModState(Status.Installed, c.toMap))
  }
  private def cState(rel: RelPath) =
    rel -> ContentState(Status.Installed, ContentKind.Component, Some(path"/$rel"))

  val rs = RepoState(Vector(
    modState(rel"/a",
      cState(rel"/file1"),
      cState(rel"/file2")
    ),
    modState(rel"/b",
      cState(rel"/file2"),
      cState(rel"/file3")
    ),
    modState(rel"/c",
      cState(rel"/file3"),
      cState(rel"/file1")
    ),
  ))

  behavior of "RepoState#at"

  it should "return mod at given path with an index" in {
    rs.at(rel"/b") shouldBe ((1, rs.orderedMods(1)))
  }

  it should "throw an error if no mod for matching path is found" in {
    an[Exception] shouldBe thrownBy {
      rs.at(rel"rel/no-can-do")
    }
  }
}
