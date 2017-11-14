package mco.io.generic

import scala.util.Try
import scalaz.ImmutableArray

import better.files.File
import com.sun.javafx.PlatformUtil
import mco.data.Path
import mco.util.Capture
import mco.util.syntax.any._

import java.io.IOException
import java.nio.file.FileAlreadyExistsException

class LocalFilesystem[F[_]: Capture] extends Filesystem[F] {
  val capture = implicitly[Capture[F]]

  private def noWinRoot(path: Path): Unit = {
    if (PlatformUtil.isWindows && path == Path.root) {
      throw new IOException("Unsupported operation at (virtual) root path")
    }
  }

  final def childrenOf(path: Path) = capture {
    val src =
      if (path == Path.root && PlatformUtil.isWindows) File.roots
      else File(path.asString).children

    src
      .map(f => Path(f.pathAsString))
      .toStream
  }

  final def getBytes(path: Path) = capture {
    ImmutableArray.fromArray(File(path.asString).byteArray)
  }

  final def setBytes(path: Path, cnt: ImmutableArray[Byte]) = capture {
    File(path.asString)
      .tap(_.parent.createDirectories())
      .touch()
      .writeByteArray(cnt.toArray)
    ()
  }

  final def mkDir(path: Path) = capture {
    File(path.asString)
      .tap { f =>
        if (f.exists) throw new FileAlreadyExistsException(path.asString)
      }
      .createDirectories()
    ()
  }

  private def noCollision(source: Path, dest: Path, op: (File, File) => Any) = capture {
    noWinRoot(source)
    noWinRoot(dest)
    val (from, to) = (File(source.asString), File(dest.asString))
    if (to.exists && from.isSameFileAs(to)) throw new IOException(
      s"Could not copy $source to $dest: files are the same"
    )
    if (from.isDirectory != to.isDirectory) throw new IOException(
      s"Could not copy file into directory. Paths are $source & $dest"
    )

    if (to.isParentOf(from) || from.isParentOf(to)) throw new IOException(
      s"Could not copy $to and $from"
    )
    op(from, to)
    ()
  }

  final def copy(source: Path, dest: Path) =
    noCollision(source, dest, _.copyTo(_, overwrite = true))

  final def move(source: Path, dest: Path) =
    noCollision(source, dest, (a, b) => {
      a.copyTo(b, overwrite = true)
      a.delete()
    })

  final def rmTree(path: Path) = capture {
    noWinRoot(path)
    File(path.asString).delete()
    ()
  }

  final def stat(path: Path) = capture {
    noWinRoot(path)
    Try(File(path.asString).attributes).toOption
  }
}
