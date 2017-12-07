package mco.core.state

import monocle.macros.Lenses

import scalaz._

@Lenses case class Stamp(
  enabled: Boolean,
  installed: Boolean = false
)

object Stamp {
  implicit val stampMonoid: Monoid[Stamp] = Monoid.instance((l, r) => Stamp(
    l.enabled || r.enabled,
    l.installed || r.installed
  ), Stamp(enabled = false))
}
