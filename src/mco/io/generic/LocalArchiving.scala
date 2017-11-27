package mco.io.generic

import scalaz._

import better.files._
import mco.core.Capture
import mco.data.paths._
import mco.util.syntax.any._
import net.sf.sevenzipjbinding._
import net.sf.sevenzipjbinding.impl.{RandomAccessFileInStream, RandomAccessFileOutStream}

import java.io.Closeable

object LocalArchiving {
  def apply[F[_]: Capture: Functor]: F[Archiving[F]] = {
    def getInStream(out: Path): IInStream =
      File(out.toString)
      .newRandomAccess()
      .pipe(new RandomAccessFileInStream(_))

    def getOutStream(out: Path): IOutStream with Closeable =
      File(out.toString)
        .touch()
        .newRandomAccess(File.RandomAccessMode.readWrite)
        .pipe(new RandomAccessFileOutStream(_) with Closeable)

    ArchivingImpl(getInStream, getOutStream)
  }
}
