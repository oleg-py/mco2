package mco.game.generic.extractors

import cats.{FlatMap, Monad}
import mco.core.paths._
import cats.implicits._

trait Extractor[F[_]] {
  def entries: fs2.Stream[F, RelPath]
  def provide(children: Set[RelPath]): fs2.Stream[F, Pointed[Path]]
}

object Extractor {
  type Provider[F[_]] = Path => F[Option[Extractor[F]]]

  def deep[F[_]: Monad](ps: Provider[F]*): Path => Extractor[F] = {
    def scan(list: List[Provider[F]])(at: Path): F[Option[Extractor[F]]] = list match {
      case Nil => none[Extractor[F]].pure[F]
      case f :: fs => f(at).flatMap {
        case None => scan(fs)(at)
        case some => some.pure[F]
      }
    }
    new Deep(scan(ps.toList))(_)
  }

  private class Deep[F[_]](
    mkExtractor: Provider[F]
  )(
    root: Path
  ) extends Extractor[F] {
    def entries: fs2.Stream[F, RelPath] = listRec(root, rel"").map(_._1)

    def provide(children: Set[RelPath]): fs2.Stream[F, Pointed[Path]] =
      listRec(root, rel"").filter { case (rel, _) => children(rel) }

    private def listRec(path: Path, rel: RelPath): fs2.Stream[F, Pointed[Path]] =
      fs2.Stream.eval(mkExtractor(path)).flatMap {
        case Some(provider) =>
          provider.entries
            .foldMap(Set(_))
            .flatMap(provider.provide)
            .flatMap { case (childRel, childAbs) =>
              listRec(childAbs, rel / childRel)
            }
        case None =>
          fs2.Stream.emit((rel / path.name, path)).covary
      }
  }
}