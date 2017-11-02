package mco.core.state

import monocle.macros.Lenses

import scalaz._
import std.tuple._
import std.anyVal._
import syntax.monoid._

@Lenses case class Stamp(
  hash: (Long, Long),
  enabled: Boolean,
  installed: Boolean = false
)

object Stamp {
  implicit val stampMonoid: Monoid[Stamp] = Monoid.instance((l, r) => Stamp(
    l.hash |+| r.hash,
    l.enabled || r.enabled,
    l.installed || r.installed
  ), Stamp((0L, 0L), enabled = false))
}
