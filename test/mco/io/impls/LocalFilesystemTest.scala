package mco.io.impls

import scala.util.Random

import mco.Tests
import mco.core.paths._
import mco.io.Filesystem
import Filesystem._
import monix.eval.Coeval

import java.nio.ByteBuffer


class LocalFilesystemTest extends Tests.SpecFixture with Tests.TempDirsFixture {
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

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#getSfStream"

  it should "open a nonexistent deeply nested file for I/O" in { dirs =>
    val target = path"${dirs.target}/deeply/nested/file"
    val chs = getSfStream(target)
    chs.runSync.apply()
    target should exist
  }

  it should "fail with error for a directory" in { dirs =>
    an [Exception] shouldBe thrownBy {
      getSfStream(dirs.src).runSync.apply()
    }
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#readFile"

  it should "read file contents as bytes" in { dirs =>
    val b = readFile(dirs.src / seg"test_folder" / seg"file1")
    b.map(toVector).runLastSync.value shouldEqual Some("Hello".getBytes.toVector)
  }

  it should "fail with error for directory" in { dirs =>
    an [Exception] shouldBe thrownBy {
      readFile(dirs.src / seg"test_folder").runSync.value
    }
  }

  it should "fail with error for non-existent file" in { dirs =>
    an [Exception] shouldBe thrownBy {
      readFile(dirs.src / seg"nonexistent.file").runSync.value
    }
  }

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#setBytes"

  it should "overwrite existing file" in { dirs =>
    val testData = Random.alphanumeric.take(Random.nextInt(50)).mkString("")
    val path = dirs.src / seg"test_folder" / seg"file1"
    path.asFile.isRegularFile shouldBe true

    writeFile(path, ByteBuffer.wrap(testData.getBytes)).apply()
    path.asFile.byteArray shouldEqual testData.getBytes
  }

  it should "create a new file with parent directories as needed" in { dirs =>
    val data = "O".getBytes
    val path = dirs.target / seg"non" / seg"existent" / seg"file.txt"
    path shouldNot exist

    writeFile(path, ByteBuffer.wrap(data)).apply()
    path should exist
  }

  it should "fail for a directory" in { dirs =>
    an [Exception] shouldBe thrownBy {
      writeFile(dirs.src, ByteBuffer.wrap("0".getBytes)).apply()
    }
  }

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

  // --------------------------------------------------------------------------

  behavior of "LocalFilesystem#mkTemp"

  it should "clear the temporary directory when operation is finished" in { _ =>
    val existence = for {
      dir <- mkTemp
      fun =  () => dir.asFile.exists
    } yield (fun(), fun)

    existence.runLastSync.value match {
      case Some((existed, exists)) =>
        existed shouldBe true
        exists() shouldBe false
      case None =>
        fail("Stream should've been nonempty")
    }
  }

  // --------------------------------------------------------------------------

  private def toVector(bb: ByteBuffer) = {
    val a = Array.ofDim[Byte](bb.remaining())
    bb.get(a)
    a.toVector
  }
}
