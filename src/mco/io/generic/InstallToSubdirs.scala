package mco.io.generic

import mco.core._
import scalaz._
import syntax.std.option._
import syntax.applicative._

import mco.core.state.ModState
import mco.data.{Key, Keyed, Path}

class InstallToSubdirs[F[_]: Applicative](target: Path)
  extends NameResolver[F]
{
  override def apply(mod: Keyed[ModState])(content: Key): F[Option[Path]] =
    (target / mod.key.unwrap / content.unwrap).some.point[F]
}
