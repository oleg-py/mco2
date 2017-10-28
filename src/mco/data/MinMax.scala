package mco.data

import scalaz.Monoid


case class MinMax(min: Int, max: Int)

object MinMax {
  implicit val minMaxMonoid: Monoid[MinMax] = Monoid.instance(
    (l, r) => MinMax(l.min min r.min, l.max max r.max),
    MinMax(Int.MaxValue, Int.MinValue)
  )
}
