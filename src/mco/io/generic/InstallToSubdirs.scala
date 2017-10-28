package mco.io.generic

import mco.core._
import scalaz._
import syntax.std.option._
import syntax.applicative._

import mco.core.state.ModState
import mco.data.{Labelled, Path}

class InstallToSubdirs[F[_]: Applicative, C <: Content.Plain](target: Path)
  extends NameResolver[F, C]
{
  override def apply(mod: Labelled[ModState], content: Labelled[C]) =
    (target / mod.key.unwrap / content.key.unwrap).some.point[F]
}
