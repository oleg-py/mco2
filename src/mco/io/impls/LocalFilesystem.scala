package mco.io.impls

import scala.language.reflectiveCalls
import scala.util.Try

import better.files._
import cats.effect.Sync
import com.sun.javafx.PlatformUtil
import mco.core.paths.Path
import mco.io.Filesystem
import mco.syntax._
import net.sf.sevenzipjbinding.impl.RandomAccessFileOutStream
import net.sf.sevenzipjbinding.{IInStream, IOutStream}

import java.io.{IOException, RandomAccessFile}
import java.net.URL
import java.nio.ByteBuffer
import java.nio.file.StandardOpenOption._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileAlreadyExistsException, StandardOpenOption}

class LocalFilesystem[F[_]: Sync] extends Filesystem[F] {
  private def noWinRoot(path: Path): Unit = {
    if (PlatformUtil.isWindows && path == Path.root) {
      throw new IOException("Unsupported operation at (virtual) root path")
    }
  }

  private def unmap(bb: ByteBuffer) =
    Try {
      val cleanable = bb.asInstanceOf[{ def cleaner(): AnyRef }]
      val cleaner = cleanable.cleaner().asInstanceOf[{ def clean(): Unit }]
      cleaner.clean()
    }

  override def readFile(path: Path): fs2.Stream[F, ByteBuffer] = {
    val acquire = capture { File(path.toString).newFileChannel }
    fs2.Stream.bracket(acquire)(
      ch => fs2.Stream.bracket(capture { ch.toMappedByteBuffer })(
        buffer => fs2.Stream(buffer),
        buffer => capture { unmap(buffer); () }
      ),
      ch => capture { ch.close() }
    )
  }

  override def writeFile(path: Path, bb: ByteBuffer): F[Unit] = capture {
    for (ch <- File(path.toString).createIfNotExists(createParents = true).fileChannel(
      Seq(WRITE, CREATE, TRUNCATE_EXISTING)
    )) {
      ch.write(bb)
    }
  }

  override def mkTemp: fs2.Stream[F, Path] = {
    val mkTemp = Sync[F].delay(File.newTemporaryDirectory("mco-lfs-"))
    fs2.Stream.bracket(mkTemp)(
      file => fs2.Stream(Path(file.pathAsString)),
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

    val acquire = capture {
      File(path.toString)
        .createIfNotExists(createParents = true)
        .newRandomAccess(File.RandomAccessMode.readWrite)
    }

    fs2.Stream.bracket(acquire)(
      raf => fs2.Stream(new BiRAFStream(raf)),
      raf => capture { raf.close() }
    )
  }

  final def childrenOf(path: Path): F[Stream[Path]] = capture {
    val src =
      if (path == Path.root && PlatformUtil.isWindows) File.roots
      else File(path.toString).children

    src
      .map(f => Path(f.pathAsString))
      .toStream
  }

  final def mkDir(path: Path): F[Unit] = capture {
    File(path.toString)
      .tap { f =>
        if (f.exists) throw new FileAlreadyExistsException(path.toString)
      }
      .createDirectories()
    ()
  }

  private def noCollision(source: Path, dest: Path, op: (File, File) => Any) = capture {
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

  final def rmTree(path: Path): F[Unit] = capture {
    noWinRoot(path)
    File(path.toString).delete()
    ()
  }

  final def stat(path: Path): F[Option[BasicFileAttributes]] = capture {
    noWinRoot(path)
    Try(File(path.toString).attributes).toOption
  }

  final def fileToUrl(p: Path): F[URL] = capture {
    File(p.toString).url
  }
}
