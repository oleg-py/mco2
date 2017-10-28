package mco.io.state

import scalaz._
import std.stream._
import std.anyVal._
import std.tuple._

import mco.util.syntax.fp._
import com.olegpy.forwarders
import mco.io.generic.Filesystem
import Filesystem._
import mco.data.Path

@forwarders trait Hashing[F[_]] {
  protected def hashFile(p: Path): F[(Long, Long)]

  final def hashAt(p: Path)(implicit F: Filesystem[F], M: Monad[F]): F[(Long, Long)] = {
    def dirHash = childrenOf(p) >>= { s => s.foldMapM(hashAt) }
    def getHash = isDirectory(p).ifM(dirHash, hashFile(p))
    exists(p).ifM(getHash, (0L, 0L).point[F])
  }
}
