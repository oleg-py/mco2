package mco.core.vars

import java.io._
import cats._
import cats.syntax.functor._
import mco.core.paths.Path
import mco.syntax._
import mco.io.Filesystem
import Filesystem._
import cats.effect.Sync

import java.nio.ByteBuffer


/**
 * Var instance that uses std Java serialization to map data
 * to a file
 *
 * @param target path to file where data will be stored
 * @tparam F effect monad with Filesystem support
 * @tparam A value type
 */
class JavaSerializableVar[F[_]: Sync: Filesystem, A](
  target: Path
)
  extends Var[F, A] {

  override def apply(): F[A] = readFile(target)
    .evalMap(bytes => capture {
      val ba = Array.ofDim[Byte](bytes.remaining())
      bytes.get(ba)
      val in = new ObjectInputStream(new ByteArrayInputStream(ba))
      val result = in.readObject().asInstanceOf[A]
      in.close()
      result
    })
    .runLastSync
    .map(_.getOrElse(sys.error("readFile returned nothing")))

  override def :=(a: A): F[Unit] = writeFile(target, {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(a)
    out.flush()
    val ba = bos.toByteArray
    out.close()
    ByteBuffer.wrap(ba)
  })
}
