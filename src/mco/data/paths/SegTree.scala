package mco.data.paths

import scalaz._
import std.map._
import std.option._
import std.vector._
import syntax.std.map._

import mco.util.syntax.fp._


sealed trait SegTree[+A] {
  import SegTree._
  final def lookup(p: RelPath): Option[SegTree[A]] =
    p.segments.foldLeft(some(this)) {
      case (Some(SegRoot(cc)), key) => cc.get(key)
      case _ => None
    }

  final def update[B >: A](p: RelPath, next: Option[SegTree[B]]): Option[SegTree[B]] = {
    def recurse(d: SegTree[B])(p: NonEmptyList[Segment]): Option[SegTree[B]] = Some {
      d match {
        case s @ SegLeaf(_) => s // Noop if the path leads past "file"
        case SegRoot(map) => p.tail.toNel
          .cata(subpath => root(map.alter(p.head) { opt =>
            recurse(opt.getOrElse(root(Map())))(subpath) // auto-create new "folders"
          }), root(map.alter(p.head) { _ => next }))
      }
    }
    p.segments.toIList.toNel.cata(recurse(this), next)
  }
}

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

