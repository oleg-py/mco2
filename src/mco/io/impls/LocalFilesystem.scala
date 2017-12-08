package mco.io.impls

import scala.util.Try

import cats._
import cats.syntax.apply._
import cats.syntax.flatMap._
import better.files._
import cats.effect.Sync
import com.sun.javafx.PlatformUtil
import mco.core.Capture
import mco.core.paths.Path
import mco.io.Filesystem
import mco.util.syntax.any._
import net.openhft.hashing.LongHashFunction
import net.sf.sevenzipjbinding.impl.RandomAccessFileOutStream
import net.sf.sevenzipjbinding.{IInStream, IOutStream}

import java.io.{IOException, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.file.FileAlreadyExistsException

class LocalFilesystem[F[_]: Sync] extends Filesystem[F] {
  private def noWinRoot(path: Path): Unit = {
    if (PlatformUtil.isWindows && path == Path.root) {
      throw new IOException("Unsupported operation at (virtual) root path")
    }
  }

  override def readFile(path: Path): fs2.Stream[F, ByteBuffer] = {
    val acquire = Capture { File(path.toString).newFileChannel }
    fs2.Stream.bracket(acquire)(
      ch => fs2.Stream.eval(Capture { ch.toMappedByteBuffer }),
      ch => Capture { ch.close() }
    )
  }

  override def writeFile(path: Path, bb: ByteBuffer): F[Unit] = Capture {
    for (ch <- File(path.toString).fileChannel) {
      ch.write(bb)
    }
  }

  override def mkTemp: fs2.Stream[F, Path] = {
    val mkTemp = Sync[F].delay(File.newTemporaryDirectory("mco-lfs-"))
    fs2.Stream.bracket(mkTemp)(
      file => fs2.Stream(Path(file.pathAsString)).covary,
      file => Sync[F].delay { file.delete(); () }
    )
  }

  override def getSfStream(path: Path): fs2.Stream[F, IInStream with IOutStream] = {
    class BiRAFStream(raf: RandomAccessFile)
      extends RandomAccessFileOutStream(raf)
      with IInStream {
      override def read(data: Array[Byte]): Int = {
        val read = raf.read(data)
        if (read == -1) 0 else read
      }
    }

    val acquire = Capture {
      File(path.toString).newRandomAccess(File.RandomAccessMode.readWrite)
    }

    fs2.Stream.bracket(acquire)(
      raf => fs2.Stream(new BiRAFStream(raf)).covary,
      raf => Capture { raf.close() }
    )
  }

  final def childrenOf(path: Path) = Capture {
    val src =
      if (path == Path.root && PlatformUtil.isWindows) File.roots
      else File(path.toString).children

    src
      .map(f => Path(f.pathAsString))
      .toStream
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

  final def fileToUrl(p: Path) = Capture {
    File(p.toString).url
  }
}
