package mco.io.generic

import scalaz._

import better.files._
import mco.core.Capture
import mco.data.paths._
import mco.util.syntax.any._
import net.sf.sevenzipjbinding._
import net.sf.sevenzipjbinding.impl.{RandomAccessFileInStream, RandomAccessFileOutStream}

import java.io.Closeable

class LocalArchiving[F[_]: Capture] extends ArchivingImpl {
  override protected def getInStream(path: Path): IInStream =
    File(path.toString)
      .newRandomAccess()
      .pipe(new RandomAccessFileInStream(_))

  override protected def getOutStream(path: Path): IOutStream with Closeable =
    File(path.toString)
      .touch()
      .newRandomAccess(File.RandomAccessMode.readWrite)
      .pipe(new RandomAccessFileOutStream(_) with Closeable)
}

