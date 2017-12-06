package mco.core.vars

import java.io._
import scalaz._
import syntax.monad._

import mco.core.Capture
import mco.core.paths.Path
import mco.io.Filesystem, Filesystem._


class JavaSerializableVar[F[_]: Monad: Capture: Filesystem, A](
  target: Path
)
  extends Var[F, A] {

  override def apply(): F[A] = getBytes(target) >>= (bytes => Capture {
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toArray))
    val result = in.readObject().asInstanceOf[A]
    in.close()
    result
  })

  override def :=(a: A): F[Unit] = setBytes(target, {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(a)
    out.flush()
    val ba = bos.toByteArray
    out.close()
    ImmutableArray.fromArray(ba)
  })
}
