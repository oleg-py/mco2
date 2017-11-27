package mco.io.generic

import scalaz._
import std.stream._

import mco.core.Mod
import mco.util.syntax.fp._
import Filesystem._
import mco.data.paths._

class SimpleModTypes[F[_]: Filesystem: Archiving: Monad] {
  def apply(mod: Path) =
    for {
      ok <- exists(mod)
      isDir <- isDirectory(mod)
      isArchive = Set(".zip", ".rar", ".7z").contains(mod.extension)
    } yield if (isDir)          Some(new FolderMod[F](mod))
            else if (isArchive) Some(new ArchiveMod[F](mod))
            else if (ok)        Some(new FileMod[F](mod))
            else                None


  def allIn(path: Path): F[Vector[(Path, Mod[F])]] =
    for {
      children <- childrenOf(path)
      mods <- children.traverse(apply)
    } yield (children zip mods)
      .collect { case (p, Some(mod)) => p -> mod }
      .toVector
}
