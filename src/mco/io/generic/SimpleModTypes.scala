package mco.io.generic

import scalaz._
import std.stream._

import mco.core.Mod
import mco.data.Path
import mco.util.syntax.fp._
import Filesystem._

class SimpleModTypes[F[_]: Filesystem: Monad] extends (Path => F[Option[Mod[F]]]) {
  override def apply(v1: Path) =
    for (isDir <- isDirectory(v1)) yield
      if (isDir) Some(new FolderMod[F](v1))
      else Some(new FileMod[F](v1))

  def allIn(path: Path): F[Vector[(Path, Mod[F])]] =
    for {
      children <- childrenOf(path)
      mods <- children.traverse(this)
    } yield (children zip mods)
      .collect { case (p, Some(mod)) => p -> mod }
      .toVector
}
