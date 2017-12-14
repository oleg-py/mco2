package mco.core.state

import mco.Tests
import mco.core.ContentKind
import mco.core.paths._


class RepoStateSpec extends Tests.Spec {
  val stamp = Stamp(enabled = true, installed = true)
  private def modState(rel: RelPath, c: (RelPath, ContentState)*): Pointed[ModState] = {
    Pointed(rel, ModState(stamp, c.toMap))
  }
  private def cState(rel: RelPath) =
    rel -> ContentState(stamp, ContentKind.Component, Some(path"/$rel"))

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

  behavior of "RepoState#hasConflicts"

  it should "return true for files that are overriden" in {
    every(List(
      rs.hasConflicts(path"/file1", 0),
      rs.hasConflicts(path"/file2", 0),
      rs.hasConflicts(path"/file3", 1),
    )) shouldBe true
  }

  it should "return false for files that not overriden" in {
    every(List(
      rs.hasConflicts(path"/file1", 1),
      rs.hasConflicts(path"/file2", 1),
      rs.hasConflicts(path"/file3", 2),
    )) shouldBe false
  }

  behavior of "RepoState#overrideIndex"

  it should "return an index of mod override comes from" in {
    rs.overrideIndex(path"/file1", 0) shouldBe Some(2)
    rs.overrideIndex(path"/file2", 0) shouldBe Some(1)
    rs.overrideIndex(path"/file3", 1) shouldBe Some(2)
  }

  it should "return None for mods that are not overriden" in {
    rs.overrideIndex(path"/file1", 1) shouldBe None
    rs.overrideIndex(path"/file2", 1) shouldBe None
    rs.overrideIndex(path"/file3", 2) shouldBe None
  }

  behavior of "RepoState#recoveryIndex"

  it should "return an index of mod override comes from" in {
    rs.recoveryIndex(path"/file1", 1) shouldBe Some(0)
    rs.recoveryIndex(path"/file1", 2) shouldBe Some(0)
    rs.recoveryIndex(path"/file2", 1) shouldBe Some(0)
    rs.recoveryIndex(path"/file3", 2) shouldBe Some(1)
  }

  it should "return None for mods that do not need recovery" in {
    rs.recoveryIndex(path"/file1", 0) shouldBe None
    rs.recoveryIndex(path"/file2", 0) shouldBe None
    rs.recoveryIndex(path"/file3", 1) shouldBe None
  }

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
