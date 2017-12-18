package mco.game.generic

import scala.collection.immutable.SortedMap

import cats.Apply
import cats.effect.Sync
import cats.syntax.all._
import mco.core.Status.{Installed, Unused}
import mco.core.paths.{Pointed, RelPath}
import mco.core.{Mod, Status}


class Installation[F[_]: Apply](
  lookupMod: Int => Pointed[Mod[F]],
  conflicts: Conflicts,
  ma: ContentTraversal[F]
) {
  def alter(
    order: Int,
    status: Status,
    content: Vector[RelPath],
  ) = {
    val Pointed(rel, mod) = lookupMod(order)
    val allResolved = content.map(ma.resolve(rel, _))
    val (hasConflicts, yy) = conflicts.resolutions(allResolved, order, status)
    val process = status match {
      case Installed =>
        mod.provide(content).evalMap { case Pointed(cRel, cPath) =>
          val target = ma.resolve(rel, cRel)
          val tracked = ma.track(cRel, target.some)
          if (hasConflicts(target)) tracked // we have overrides, just mark as installed
          else tracked *> ma.copy(cPath, target)
        }
      case Unused =>
        fs2.Stream.emits(content zip allResolved).evalMap { case (cRel, at) =>
          val tracked = ma.track(cRel, none)
          if (hasConflicts(at)) tracked // we will override this file as we recover
          else tracked *> ma.remove(at)
        }
    }
    process ++ doRecovery(yy)
  }

  private def doRecovery(map: SortedMap[Int, Set[RelPath]]): fs2.Stream[F, Unit] =
    for {
      entry <- fs2.Stream.emits(map.toSeq)
      (idx, set) = entry
      Pointed(rel, mod) = lookupMod(idx)
      child <- mod.provide(set.toVector)
      target = ma.resolve(rel, child.key)
      _     <- fs2.Stream.eval(ma.copy(child.get, target))
    } yield ()
}
