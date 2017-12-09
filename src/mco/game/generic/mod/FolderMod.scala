package mco.game.generic.mod

import cats._
import cats.instances.stream._
import cats.syntax.all._
import cats.data.StateT
import mouse.all._

import mco.core._
import mco.core.paths._
import mco.io.Filesystem, Filesystem._
import mco.syntax._

class FolderMod[F[_]: Filesystem: Monad](
  override val backingFile: Path
) extends Mod[F]
{

  private type DataM = Map[RelPath, (Path, RelPath)]

  private def toState[A](fa: F[A]): StateT[F, DataM, A] =
    StateT(s => fa.tupleLeft(s))

  private def scanDeepRec(fa: F[Stream[Path]]) =
    toState(fa).flatMap(_ traverse_ scanDeep)

  private def scanDeep(path: Path): StateT[F, DataM, Unit] = {
    implicit def applicativeMonoid[M: Monoid]: Monoid[F[M]] = Applicative.monoid[F, M]

    for {
      isDir   <- isDirectory(path).pipe(toState)
      _       <- scanDeepRec(isDir ?? childrenOf(path))
      key     =  path relTo backingFile
      _       <- if (isDir) StateT.pure[F, DataM, Unit](unit)
                 else StateT.modify[F, DataM](_.updated(key, (path, key)))
    } yield ()
  }

  private val structureF: F[DataM] =
    scanDeepRec(childrenOf(backingFile)).runS(Map())

  override def list: F[Vector[RelPath]] = structureF.map { data =>
    data.map { case (_, (_, c)) => c } .toVector
  }


//  override def provide(content: Vector[RelPath]): InTemp[F, Map[RelPath, Path]] = InTemp {
//    for {
//      data <- structureF
//    } yield content
//      .collect(data)
//      .map(_.swap)
//      .toMap
//  }
  override def provide(content: Vector[RelPath]): fs2.Stream[F, Pointed[Path]] =
    fs2.Stream.eval(structureF).flatMap {
      dataM =>
        val filtered = content.collect(dataM).map { case (p, r) => Pointed(r, p) }
        fs2.Stream.emits(filtered).covary
    }
}
