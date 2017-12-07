package mco.core.state

import monocle.macros.Lenses

import cats._

@Lenses case class Stamp(
  enabled: Boolean,
  installed: Boolean = false
)

object Stamp {
  implicit val stampMonoid: Monoid[Stamp] = new Monoid[Stamp] {
    override def empty: Stamp = Stamp(enabled = false)

    override def combine(x: Stamp, y: Stamp): Stamp = Stamp(
      x.enabled || y.enabled,
      x.installed || y.installed
    )
  }
}
