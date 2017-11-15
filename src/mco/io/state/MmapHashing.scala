package mco.io.state

import better.files._
import mco.data.Path
import mco.util.Capture
import net.openhft.hashing.LongHashFunction

class MmapHashing[F[_]: Capture] extends Hashing[F] {
  final protected def hashFile(p: Path) = Capture {
    val file = File(p.asString)
    for (ch <- file.fileChannel) yield {
      val mm = ch.toMappedByteBuffer
      val hi = LongHashFunction.xx(0L).hashBytes(mm)
      val lo = LongHashFunction.farmNa(0L).hashBytes(mm)
      (hi, lo)
    }
  }
}
