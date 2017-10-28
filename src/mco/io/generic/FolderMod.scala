package mco.io.generic

import scalaz._
import std.stream._
import syntax.monoid._
import syntax.either._
import syntax.id._
import syntax.std.boolean._

import mco.core._
import mco.util.syntax.fp._
import mco.util.syntax.any._
import Filesystem._
import mco.data.{Key, Labelled, Path}

class FolderMod[F[_]: Filesystem: Monad](self: Path)
  extends Mod[F]
{
  import FolderMod._
  implicit def mapRightMonoid[K, V]: Monoid[Map[K, V]] = Monoid.instance(_ ++ _, Map())

  private def toWriter[A](fa: F[A]): WriterT[F, DataM, A] =
    WriterT.put(fa)(mzero[DataM])

  private def scanDeepRec(fa: F[Stream[Path]]) =
    toWriter(fa).flatMap(_ traverse scanDeep)

  private def scanDeep(path: Path): WriterT[F, DataM, Labelled[Content]] = {
    import Content.{Component, Container}
    import mco.util.instances.monoidForApplicative

    for {
      isDir   <- isDirectory(path) |> toWriter
      inner   <- scanDeepRec(isDir ?? childrenOf(path))
      key     =  Key(path relStringTo self)
      kind    =  isDir.fold(Container(inner.toVector), Component)
      content =  Labelled(key, path.name, kind)
      info    =  Map(key -> ((path, content)))
      _       <- WriterT.put(unit.point[F])(info)
    } yield content
  }

  /*_*/
  private val structureF: F[Map[Key, (Path, Labelled[Content])]] =
    scanDeepRec(childrenOf(self)).written
  /*_*/

  override def list = structureF.map { data =>
    data.map { case (_, (_, c)) => c } .toVector
  }

  override def provide(contents: Vector[Labelled[Content.Plain]]) = _ => {
    for {
      data <- structureF
    } yield contents
      .flatMap(c => data.get(c.key))
      .map { case (path, content) => content.key -> path }
      .toMap
  }
}

object FolderMod {
  type DataM = Map[Key, (Path, Labelled[Content])]
}
