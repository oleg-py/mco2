package mco.io.generic

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.tuple._
import scalaz.syntax.monoid._

import mco.core._
import mco.core.state.ModState
import mco.data.{Labelled, Path}
import mco.io.generic.Filesystem._
import mco.util.syntax.fp._

import java.util.UUID

class MangleNames[F[_]: Filesystem: Monad, C <: Content.Plain](target: Path)
  extends NameResolver[F, C]
{
  private val uuidName = new UUID(_: Long, _: Long).toString
  private val step = (1L, 3L)
  override def apply(mod: Labelled[ModState], content: Labelled[C]) = {
    // TODO - extract ext. without dummy path
    val ext = (Path.root / content.key.unwrap).extension
    val toName = uuidName.tupled.andThen(_ ++ ext)

    def nextFreeName(hash: (Long, Long)): F[Path] = {
      val candidate = target / toName(hash)
      exists(candidate).ifM(
        nextFreeName(hash |+| step),
        candidate.point[F]
      )
    }

    mod.get.contents.lookup(content.key)
      .map(_.stamp.hash)
      .traverse(nextFreeName)
  }
}
