package mco.io.generic

import scala.util.Random
import scalaz.Id._
import scalaz._

import better.files.File
import mco.Tests
import mco.data.paths._
import mco.io.generic.Filesystem._
import mco.util.Capture
import mco.util.syntax.any._


class LocalFilesystemTest extends Tests.SyncFixture with Tests.BetterFilesHelpers {
  implicit val captureImpure: Capture[Id] = new Capture[Id] {
    override def apply[A](a: => A): Id[A] = a
  }
  implicit val impureFs: Filesystem[Id] = new LocalFilesystem[Id]

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#childrenOf"

  it should "list elements of directory" in { dirs =>
    val chs = childrenOf(dirs.src).toSet

    chs.map(_.name) should contain only (
      seg"test_folder",
      seg"test_archive.7z",
      seg"test_archive.rar",
      seg"test_archive.zip"
    )
  }

  it should "return empty list for empty directory" in { dirs =>
    dirs.target shouldBe empty

    childrenOf(dirs.target) shouldBe empty
  }

  it should "fail with error for a file" in { dirs =>
    an [Exception] shouldBe thrownBy {
      childrenOf(dirs.src / seg"test_archive.7z")
    }
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#getBytes"

  it should "read file contents as bytes" in { dirs =>
    val b = getBytes(dirs.src / seg"test_folder" / seg"file1")
    b.toArray shouldEqual "Hello".getBytes
  }

  it should "fail with error for directory" in { dirs =>
    an [Exception] shouldBe thrownBy {
      getBytes(dirs.src / seg"test_folder")
    }
  }

  it should "fail with error for non-existent file" in { dirs =>
    an [Exception] shouldBe thrownBy {
      getBytes(dirs.src / seg"nonexistent.file")
    }
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#setBytes"

  it should "overwrite existing file" in { dirs =>
    val testData = Random.alphanumeric.take(Random.nextInt(50)).mkString("")
    val path = dirs.src / seg"test_folder" / seg"file1"
    path.asFile.isRegularFile shouldBe true

    setBytes(path, ImmutableArray.fromArray(testData.getBytes))
    path.asFile.byteArray shouldEqual testData.getBytes
  }

  it should "create a new file with parent directories as needed" in { dirs =>
    val data = ImmutableArray.fromArray("O".getBytes)
    val path = dirs.target / seg"non" / seg"existent" / seg"file.txt"
    path shouldNot exist

    setBytes(path, data)
    path should exist
  }

  it should "fail for a directory" in { dirs =>
    an [Exception] shouldBe thrownBy {
      setBytes(dirs.src, ImmutableArray.fromArray("0".getBytes))
    }
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#mkDir"

  it should "create a directory with all parents if not exists" in { dirs =>
    val path = dirs.target / seg"non" / seg"existent" / seg"directory"
    path shouldNot exist

    mkDir(path)
    path.asFile.isDirectory shouldBe true
  }

  it should "fail if a directory already exists" in { dirs =>
    val path = dirs.target / seg"non" / seg"existent" / seg"directory"
    path.asFile.createDirectories()

    an [Exception] shouldBe thrownBy {
      mkDir(path)
    }
  }

  it should "fail if a file is at the given path" in { dirs =>
    val path = dirs.src / seg"test_folder" / seg"file1"
    path.asFile.isRegularFile shouldBe true

    an [Exception] shouldBe thrownBy {
      mkDir(path)
    }
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#copy on a file"

  it should "make a copy available at destination" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.target / seg"out"
    copy(from, to)
    all (Seq(from, to).map(_.asFile)) should exist

    val Seq(cn1, cn2) = Seq(from, to).map(_.asFile.byteArray)
    cn1 shouldEqual cn2
  }

  it should "overwrite destination file if it exists" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.src / seg"test_folder" / seg"file1"
    all (Seq(from, to)) should exist

    copy(from, to)
    val Seq(cn1, cn2) = Seq(from, to).map(_.asFile.byteArray)
    cn1 shouldEqual cn2
  }

  it should "fail if a directory exists at given path" in { dirs =>
    an [Exception] shouldBe thrownBy {
      copy(dirs.src / seg"test_folder" / seg"file2", dirs.target)
    }
  }

  it should "fail trying to copy file into itself, without destroying" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val origContent = from.asFile.byteArray
    an [Exception] shouldBe thrownBy {
      copy(from, from)
    }
    from should exist
    from.asFile.byteArray shouldEqual origContent
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#copy on a directory"

  it should "copy its contents to a new location" in { dirs =>
    val from = dirs.src / seg"test_folder"
    val to = dirs.target

    copy(from, to)

    from should exist
    (to / seg"file1").asFile.contentAsString shouldEqual "Hello"
  }

  it should "merge its contents with contents at given path" in { dirs =>
    val from = dirs.src
    val to = dirs.target
    val oldFile = (to.asFile / "deep/nested/subdir")
      .createDirectories()
      ./("test.txt")
      .write("poke")
    copy(from, to)
    oldFile should exist
  }

  it should "fail if a file exists at given path" in { dirs =>
    val from = dirs.src
    val to = dirs.target / seg"foo.txt"
    val text = "I'm a file!"
    to.asFile.touch().write(text)
    an [Exception] shouldBe thrownBy {
      copy(from, to)
    }
    to.asFile.contentAsString shouldEqual text
  }

  it should "fail if copying into a path inside itself" in { dirs =>
    val from = dirs.src
    val to = dirs.src / seg"test_folder"
    an [Exception] shouldBe thrownBy {
      copy(from, to)
    }
    val files = Seq(to, to / seg"file2")
    all (files) should exist
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#move"

  it should "remove original element" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.target / seg"foo"

    move(from, to)

    from shouldNot exist
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#move on a file"

  it should "make a file available at destination" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.target / seg"foo"

    move(from, to)
    to should exist
  }

  it should "overwrite destination file if it exists" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val contents = from.asFile.byteArray
    val to = dirs.target / seg"foo"

    to.asFile.touch().write("Garbalx")

    move(from, to)

    to.asFile.byteArray shouldEqual contents
  }

  it should "fail if a directory exists at given path" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.target / seg"foo" / seg"bar"

    to.asFile.createDirectories()

    an[Exception] shouldBe thrownBy {
      move(from, to)
    }

    from should exist
    to.asFile.isDirectory shouldBe true
  }

  it should "fail trying to copy file into itself, without destroying" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val contents = from.asFile.byteArray

    an [Exception] shouldBe thrownBy {
      move(from, from)
    }

    from.asFile.byteArray shouldEqual contents
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#move on a folder"

  it should "move its contents to a new location" in { dirs =>
    val from = dirs.src / seg"test_folder"
    val to = dirs.target

    move(from, to)

    from shouldNot exist
    (to / seg"file1").asFile.contentAsString shouldEqual "Hello"
  }

  it should "merge its contents with contents at given path" in { dirs =>
    val from = dirs.src
    val to = dirs.target
    val oldFile = (to.asFile / "deep/nested/subdir")
      .createDirectories()
      ./("test.txt")
      .write("poke")
    move(from, to)
    oldFile should exist
  }

  it should "fail if a file exists at given path" in { dirs =>
    val from = dirs.src
    val to = dirs.target / seg"foo.txt"
    val text = "I'm a file!"
    to.asFile.touch().write(text)
    an [Exception] shouldBe thrownBy {
      move(from, to)
    }
    to.asFile.contentAsString shouldEqual text
  }

  it should "fail if moving into a path inside itself" in { dirs =>
    val from = dirs.src
    val to = dirs.src / seg"test_folder"
    an [Exception] shouldBe thrownBy {
      move(from, to)
    }
    val files = Seq(to, to / seg"file2")
    all (files) should exist
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#rmTree"

  it should "remove a file, given a path to it" in { dirs =>
    val p = dirs.src / seg"test_archive.zip"
    p.asFile.isRegularFile shouldBe true
    rmTree(p)
    p shouldNot exist
  }

  it should "remove a directory with children, given a path to it" in { dirs =>
    val p = dirs.src / seg"test_folder"
    p.asFile.isDirectory shouldBe true
    rmTree(p)
    p shouldNot exist
  }

  it should "fail, given a path to non-existent object" in { dirs =>
    val p = dirs.src / rel"does/not/exist"
    p shouldNot exist
    an [Exception] shouldBe thrownBy {
      rmTree(p)
    }
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#hashAt on a file"

  it should "hash it contents" in { dirs =>
    hashAt(dirs.src / rel"test_folder/file1") shouldEqual helloHash
  }

  it should "hash independently of file location" in { dirs =>
    val target = dirs.src / seg"test_file"
    target.asFile.write("Hello")
    hashAt(target) shouldEqual helloHash
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#hashAt on a directory"

  it should "generate meaningful hash" in { dirs =>
    hashAt(dirs.src) shouldEqual testDirHash
  }

  // --------------------------------------------------------------------------

  val helloHash   = ( 753694413698530628L, 1860348619331993311L)
  val testDirHash = (3969179940749501790L, 1702180032275553555L)

  // --------------------------------------------------------------------------

  case class Dirs(src: Path, target: Path)
  override type FixtureParam = Dirs
  override def withFixture(test: OneArgTest) = {
    for (from <- File.temporaryDirectory("mco-io-from")) yield {
      getClass
        .getResource("/mco/io/algebras/fixture")
        .toURI
        .pipe(File(_))
        .copyTo(from, overwrite = true)

      for (to <- File.temporaryDirectory("mco-io-to")) yield {
        withFixture(test.toNoArgTest(Dirs(from.asPath, to.asPath)))
      }
    }
  }
}
