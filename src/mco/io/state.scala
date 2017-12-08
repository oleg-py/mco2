package mco.io

import cats._
import cats.implicits._

import mco.core.paths.Path
import mco.core.{Content, Mod}
import mco.core.state.{ContentState, ModState, Stamp}

object state {
  def initMod[F[_]: Filesystem: Monad](mod: Mod[F]): F[ModState] = ???
}
