package mco.game.generic.store

import mco.core.paths._
import mco.game.generic.extractors.Extractor
import cats.instances.set._

case class Mod[F[_]](backingFile: Path, data: Extractor[F]) {
  def label = backingFile.name.toString

  def provideAll: fs2.Stream[F, Pointed[Path]] =
    data.entries.foldMap(Set(_)).flatMap(data.provide)
}
