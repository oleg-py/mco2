package mco.data.paths

import scalaz._
import std.map._

import mco.util.syntax.fp._


sealed trait SegTree[+A]
object SegTree {
  case class SegLeaf[A](value: A) extends SegTree[A]
  case class SegRoot[A](map: Map[Segment, SegTree[A]]) extends SegTree[A]

  def leaf[A](value: A): SegTree[A] = SegLeaf(value)
  def root[A](map: Map[Segment, SegTree[A]]): SegTree[A] = SegRoot(map)

  implicit val segTreeTraverse: Traverse[SegTree] = new Traverse[SegTree] {
    override def traverseImpl[G[_], A, B](fa: SegTree[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[SegTree[B]] = {
      fa match {
        case SegLeaf(a) => f(a).map(leaf)
        case SegRoot(map) => map.traverse(subtree => traverseImpl(subtree)(f)).map(root)
      }
    }
  }
  /*_*/
  implicit def segTreeMonoid[A: Monoid]: Monoid[SegTree[A]] = new Monoid[SegTree[A]] {
    override def zero: SegTree[A] = leaf(mzero[A])
    override def append(f1: SegTree[A], f2: => SegTree[A]): SegTree[A] =
      (f1, f2) match {
        case (SegLeaf(a), SegLeaf(b)) => leaf(a |+| b)
        case (a @ SegLeaf(_), SegRoot(bs)) => root(bs.mapValues(append(a, _: SegTree[A])))
        case (SegRoot(as), b @ SegLeaf(_)) => root(as.mapValues(append(_: SegTree[A], b)))
        case (SegRoot(as), SegRoot(bs)) => root(as |+| bs)
      }
  }
}

