package mco.util.syntax

import scalaz.syntax._
import std.{ToOptionIdOps, ToOptionOps}


/**
 * Scalaz has functor syntax in both traverse ops & monad ops
 * which prevents it from working. This is probably most useful
 * combination of imports that is not `syntax.all` yet.
 */
object fp extends ToMonadErrorOps
  with ToTraverseOps
  with ToMonoidOps
  with ToOptionOps
  with ToOptionIdOps
  with ToComonadOps
