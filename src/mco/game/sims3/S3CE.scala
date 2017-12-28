package mco.game.sims3

import cats.effect.Sync
import mco.core.paths.Path
import mco.syntax._
import scala.sys.process._

// @todo this class assumes that Path.toString gives a platform path
class S3CE[F[_]: Sync](tool: Path) {
  def apply(path: Path): F[Unit] = capture {
    Process(Seq(
      "cmd", "/c", tool.toString, path.toString
    )).!
    ()
  }
}
