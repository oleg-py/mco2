package mco.io.generic

import scalaz._

import better.files._
import mco.core.Capture
import mco.util.syntax.any._
import net.sf.sevenzipjbinding.impl.{RandomAccessFileInStream, RandomAccessFileOutStream}

import java.io.Closeable

class LocalArchiving[F[_]: Capture] extends ArchivingImpl(
  (path) => {
    File(path.toString)
      .newRandomAccess()
      .pipe(new RandomAccessFileInStream(_))
  },
  (path) => {
    File(path.toString)
      .createIfNotExists(createParents = true)
      .newRandomAccess(File.RandomAccessMode.readWrite)
      .pipe(new RandomAccessFileOutStream(_) with Closeable)
  }
)

