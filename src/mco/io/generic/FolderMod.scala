package mco.io.generic

import scalaz._
import std.stream._
import syntax.id._
import syntax.std.boolean._

import mco.core._
import mco.util.syntax.fp._
import mco.util.syntax.any._
import Filesystem._
import mco.data.{Key, Keyed, Path}
import mco.util.instances.mapRightMonoid

class FolderMod[F[_]: Filesystem: Monad](self: Path)
  extends Mod[F]
{
  import FolderMod._

  override val label: String = self.name

  private def toWriter[A](fa: F[A]): WriterT[F, DataM, A] =
    WriterT.put(fa)(mzero[DataM])

  private def scanDeepRec(fa: F[Stream[Path]]) =
    toWriter(fa).flatMap(_ traverse scanDeep)

  private def scanDeep(path: Path): WriterT[F, DataM, Keyed[Content]] = {
    import Content.{Component, Container}
    import mco.util.instances.monoidForApplicative

    for {
      isDir   <- isDirectory(path) |> toWriter
      inner   <- scanDeepRec(isDir ?? childrenOf(path))
      key     =  Key(path relStringTo self)
      kind    =  isDir.fold(Container(inner.toVector), Component)
      content =  Keyed(key, kind)
      info    =  Map(key -> ((path, content)))
      _       <- WriterT.put(unit.point[F])(info)
    } yield content
  }

  /*_*/
  private val structureF: F[Map[Key, (Path, Keyed[Content])]] =
    scanDeepRec(childrenOf(self)).written
  /*_*/

  override def list = structureF.map { data =>
    data.map { case (_, (_, c)) => c } .toVector
  }

  override def provide(contents: Vector[Key]) = _ => {
    for {
      data <- structureF
    } yield contents
      .collect(data)
      .map { case (path, content) => content.key -> path }
      .toMap
  }
}

object FolderMod {
  type DataM = Map[Key, (Path, Keyed[Content])]
}
