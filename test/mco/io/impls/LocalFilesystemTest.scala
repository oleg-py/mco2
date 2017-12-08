package mco.io.impls

import scala.util.Random

import cats._
import better.files.File
import mco.Tests
import mco.core.paths._
import mco.io.Filesystem
import Filesystem._
import mco.util.syntax.any._
import monix.eval.Coeval


class LocalFilesystemTest extends Tests.SpecFixture {
  implicit val fs: Filesystem[Coeval] = new LocalFilesystem[Coeval]

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#childrenOf"

  it should "list elements of directory" in { dirs =>
    val chs = childrenOf(dirs.src).value

    chs.map(_.name) should contain only (
      seg"test_folder",
      seg"test_archive.7z",
      seg"test_archive.rar",
      seg"test_archive.zip"
    )
  }

  it should "return empty list for empty directory" in { dirs =>
    dirs.target shouldBe empty

    childrenOf(dirs.target).value shouldBe empty
  }

  it should "fail with error for a file" in { dirs =>
    an [Exception] shouldBe thrownBy {
      childrenOf(dirs.src / seg"test_archive.7z").value
    }
  }

/*  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#getBytes"

  it should "read file contents as bytes" in { dirs =>
    val b = getBytes(dirs.src / seg"test_folder" / seg"file1")
    b shouldEqual "Hello".getBytes
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

    setBytes(path, testData.getBytes)
    path.asFile.byteArray shouldEqual testData.getBytes
  }

  it should "create a new file with parent directories as needed" in { dirs =>
    val data = "O".getBytes
    val path = dirs.target / seg"non" / seg"existent" / seg"file.txt"
    path shouldNot exist

    setBytes(path, data)
    path should exist
  }

  it should "fail for a directory" in { dirs =>
    an [Exception] shouldBe thrownBy {
      setBytes(dirs.src, "0".getBytes)
    }
  }*/

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#mkDir"

  it should "create a directory with all parents if not exists" in { dirs =>
    val path = dirs.target / seg"non" / seg"existent" / seg"directory"
    path shouldNot exist

    mkDir(path).value
    path.asFile.isDirectory shouldBe true
  }

  it should "fail if a directory already exists" in { dirs =>
    val path = dirs.target / seg"non" / seg"existent" / seg"directory"
    path.asFile.createDirectories()

    an [Exception] shouldBe thrownBy {
      mkDir(path).value
    }
  }

  it should "fail if a file is at the given path" in { dirs =>
    val path = dirs.src / seg"test_folder" / seg"file1"
    path.asFile.isRegularFile shouldBe true

    an [Exception] shouldBe thrownBy {
      mkDir(path).value
    }
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#copy on a file"

  it should "make a copy available at destination" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.target / seg"out"
    copy(from, to).value
    all (Seq(from, to).map(_.asFile)) should exist

    val Seq(cn1, cn2) = Seq(from, to).map(_.asFile.byteArray)
    cn1 shouldEqual cn2
  }

  it should "overwrite destination file if it exists" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.src / seg"test_folder" / seg"file1"
    all (Seq(from, to)) should exist

    copy(from, to).value
    val Seq(cn1, cn2) = Seq(from, to).map(_.asFile.byteArray)
    cn1 shouldEqual cn2
  }

  it should "fail if a directory exists at given path" in { dirs =>
    an [Exception] shouldBe thrownBy {
      copy(dirs.src / seg"test_folder" / seg"file2", dirs.target).value
    }
  }

  it should "fail trying to copy file into itself, without destroying" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val origContent = from.asFile.byteArray
    an [Exception] shouldBe thrownBy {
      copy(from, from).value
    }
    from should exist
    from.asFile.byteArray shouldEqual origContent
  }

  it should "correctly create parents of target" in { dirs =>
    val from = dirs.src / rel"test_folder/file2"
    val origContent = from.asFile.byteArray
    val to = dirs.target / rel"deeply/nested/nonexistent/dir"
    noException shouldBe thrownBy {
      copy(from, to).value
    }
    to should exist
    to.asFile.byteArray shouldEqual origContent
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#copy on a directory"

  it should "copy its contents to a new location" in { dirs =>
    val from = dirs.src / seg"test_folder"
    val to = dirs.target

    copy(from, to).value

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
    copy(from, to).value
    oldFile should exist
  }

  it should "fail if a file exists at given path" in { dirs =>
    val from = dirs.src
    val to = dirs.target / seg"foo.txt"
    val text = "I'm a file!"
    to.asFile.touch().write(text)
    an [Exception] shouldBe thrownBy {
      copy(from, to).value
    }
    to.asFile.contentAsString shouldEqual text
  }

  it should "fail if copying into a path inside itself" in { dirs =>
    val from = dirs.src
    val to = dirs.src / seg"test_folder"
    an [Exception] shouldBe thrownBy {
      copy(from, to).value
    }
    val files = Seq(to, to / seg"file2")
    all (files) should exist
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#move"

  it should "remove original element" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.target / seg"foo"

    move(from, to).value

    from shouldNot exist
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#move on a file"

  it should "make a file available at destination" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.target / seg"foo"

    move(from, to).value
    to should exist
  }

  it should "overwrite destination file if it exists" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val contents = from.asFile.byteArray
    val to = dirs.target / seg"foo"

    to.asFile.touch().write("Garbalx")

    move(from, to).value

    to.asFile.byteArray shouldEqual contents
  }

  it should "fail if a directory exists at given path" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val to = dirs.target / seg"foo" / seg"bar"

    to.asFile.createDirectories()

    an[Exception] shouldBe thrownBy {
      move(from, to).value
    }

    from should exist
    to.asFile.isDirectory shouldBe true
  }

  it should "fail trying to copy file into itself, without destroying" in { dirs =>
    val from = dirs.src / seg"test_folder" / seg"file2"
    val contents = from.asFile.byteArray

    an [Exception] shouldBe thrownBy {
      move(from, from).value
    }

    from.asFile.byteArray shouldEqual contents
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#move on a folder"

  it should "move its contents to a new location" in { dirs =>
    val from = dirs.src / seg"test_folder"
    val to = dirs.target

    move(from, to).value

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
    move(from, to).value
    oldFile should exist
  }

  it should "fail if a file exists at given path" in { dirs =>
    val from = dirs.src
    val to = dirs.target / seg"foo.txt"
    val text = "I'm a file!"
    to.asFile.touch().write(text)
    an [Exception] shouldBe thrownBy {
      move(from, to).value
    }
    to.asFile.contentAsString shouldEqual text
  }

  it should "fail if moving into a path inside itself" in { dirs =>
    val from = dirs.src
    val to = dirs.src / seg"test_folder"
    an [Exception] shouldBe thrownBy {
      move(from, to).value
    }
    val files = Seq(to, to / seg"file2")
    all (files) should exist
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#rmTree"

  it should "remove a file, given a path to it" in { dirs =>
    val p = dirs.src / seg"test_archive.zip"
    p.asFile.isRegularFile shouldBe true
    rmTree(p).value
    p shouldNot exist
  }

  it should "remove a directory with children, given a path to it" in { dirs =>
    val p = dirs.src / seg"test_folder"
    p.asFile.isDirectory shouldBe true
    rmTree(p).value
    p shouldNot exist
  }

  it should "fail, given a path to non-existent object" in { dirs =>
    val p = dirs.src / rel"does/not/exist"
    p shouldNot exist
    an [Exception] shouldBe thrownBy {
      rmTree(p).value
    }
  }

/*  // --------------------------------------------------------------------------

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

  behavior of "LocalFilesystem#runTmp on a directory"

  it should "provide the dir until nested op finish" in { _ =>
    val op: InTemp[Id, Path] = InTemp.WithTemp(p => p: Id[Path])
    val nested = (1 to 50).foldLeft(op)((op2, _) => op2.andThen(p => p: Id[Path]))
    //noinspection ConvertibleToMethodValue
    op.andThen(exists(_)).runFS shouldBe true
  }*/

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
