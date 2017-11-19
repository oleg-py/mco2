package mco.util

import scalaz.ImmutableArray

import java.io._


object serialize {
  def to[A <: Serializable](a: A): ImmutableArray[Byte] = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(a)
    out.flush()
    val ba = bos.toByteArray
    out.close()
    ImmutableArray.fromArray(ba)
  }

  def from[A <: Serializable](bytes: ImmutableArray[Byte]) = {
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toArray))
    val result = in.readObject().asInstanceOf[A]
    in.close()
    result
  }
}
