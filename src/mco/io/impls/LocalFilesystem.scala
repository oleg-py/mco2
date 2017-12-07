package mco.io.impls

import scala.util.Try
import cats._
import cats.syntax.apply._
import cats.syntax.flatMap._

import better.files._
import com.sun.javafx.PlatformUtil
import mco.core.Capture
import mco.core.paths.Path
import mco.io.{Archiving, Filesystem}
import mco.util.syntax.any._
import net.openhft.hashing.LongHashFunction

import java.io.IOException
import java.nio.file.FileAlreadyExistsException

class LocalFilesystem[F[_]: Capture: FlatMap] extends Filesystem[F] {
  override val archiving: Archiving[F] = new LocalArchiving[F]

  private def noWinRoot(path: Path): Unit = {
    if (PlatformUtil.isWindows && path == Path.root) {
      throw new IOException("Unsupported operation at (virtual) root path")
    }
  }

  final def childrenOf(path: Path) = Capture {
    val src =
      if (path == Path.root && PlatformUtil.isWindows) File.roots
      else File(path.toString).children

    src
      .map(f => Path(f.pathAsString))
      .toStream
  }

  final def getBytes(path: Path) = Capture {
    File(path.toString).byteArray
  }

  final def setBytes(path: Path, cnt: Array[Byte]) = Capture {
    File(path.toString)
      .createIfNotExists(createParents = true)
      .writeByteArray(cnt)
    ()
  }

  final def mkDir(path: Path) = Capture {
    File(path.toString)
      .tap { f =>
        if (f.exists) throw new FileAlreadyExistsException(path.toString)
      }
      .createDirectories()
    ()
  }

  private def noCollision(source: Path, dest: Path, op: (File, File) => Any) = Capture {
    noWinRoot(source)
    noWinRoot(dest)
    val (from, to) = (File(source.toString), File(dest.toString))
    if (to.exists && from.isSameFileAs(to)) throw new IOException(
      s"Could not copy $source to $dest: files are the same"
    )
    if (from.isDirectory != to.isDirectory) throw new IOException(
      s"Could not copy file into directory. Paths are $source & $dest"
    )

    if (to.isParentOf(from) || from.isParentOf(to)) throw new IOException(
      s"Could not copy $to and $from"
    )
    to.parent.createDirectories()
    op(from, to)
    ()
  }

  final def copy(source: Path, dest: Path): F[Unit] =
    noCollision(source, dest, _.copyTo(_, overwrite = true))

  final def move(source: Path, dest: Path): F[Unit] =
    noCollision(source, dest, (a, b) => {
      a.copyTo(b, overwrite = true)
      a.delete()
    })

  final def rmTree(path: Path) = Capture {
    noWinRoot(path)
    File(path.toString).delete()
    ()
  }

  final def stat(path: Path) = Capture {
    noWinRoot(path)
    Try(File(path.toString).attributes).toOption
  }

  final def runTmp[A](f: Path => F[A]) = Capture {
    val tmpDir = File.newTemporaryDirectory("mco-")
    f(Path(tmpDir.pathAsString)) <* Capture {
      tmpDir.delete(swallowIOExceptions = true)
    }
  }.flatten

  final protected[mco] def hashFile(p: Path) = Capture {
    val file = File(p.toString)
    for (ch <- file.fileChannel) yield {
      val mm = ch.toMappedByteBuffer
      val hi = LongHashFunction.xx(0L).hashBytes(mm)
      val lo = LongHashFunction.farmNa(0L).hashBytes(mm)
      (hi, lo)
    }
  }

  final def fileToUrl(p: Path) = Capture {
    File(p.toString).url
  }
}
