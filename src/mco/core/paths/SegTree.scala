package mco.core.paths

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import mouse.all._
import mco.syntax._

sealed trait SegTree[+A] {
  import SegTree._
  final def lookup(p: RelPath): Option[SegTree[A]] =
    p.segments.foldLeft(this.some) {
      case (Some(SegRoot(cc)), key) => cc.get(key)
      case _ => None
    }

  final def lookupVal(p: RelPath): Option[A] =
    lookup(p).collect { case SegLeaf(a) => a }

  final def update[B >: A](p: RelPath, next: Option[SegTree[B]]): Option[SegTree[B]] = {
    def recurse(d: SegTree[B])(p: NonEmptyList[Segment]): Option[SegTree[B]] = Some {
      d match {
        case s @ SegLeaf(_) => s // Noop if the path leads past "file"
        case SegRoot(map) => p.tail.toNel
          .cata(subpath => root(map.alter(p.head, { opt =>
            recurse(opt.getOrElse(root()))(subpath) // auto-create new "folders"
          })), root(map.alter(p.head, { _ => next })))
      }
    }
    p.segments.toList.toNel.cata(recurse(this), next)
  }

  final def children: Stream[Pointed[A]] = this match {
    case SegLeaf(a) => Stream(Pointed(rel"", a))
    case SegRoot(map) => map.iterator
      .flatMap { case (k, v) =>
        for (Pointed(p, a) <- v.children) yield Pointed(rel"$k/$p", a)
      }
      .toStream
  }
}

object SegTree {
  def fromFoldable[F[_]: Foldable, A](f: F[Pointed[A]]): SegTree[A] =
    f.foldLeftM(root[A]()) { case (b, Pointed(p, a)) => b.update(p, Some(leaf(a))) }
      .getOrElse(root[A]())

  case class SegLeaf[A](value: A) extends SegTree[A]
  case class SegRoot[A](map: Map[Segment, SegTree[A]]) extends SegTree[A]

  def leaf[A](value: A): SegTree[A] = SegLeaf(value)
  def root[A](map: Map[Segment, SegTree[A]]): SegTree[A] = SegRoot(map)
  def root[A](map: (Segment, SegTree[A])*): SegTree[A] = SegRoot(map.toMap)
}

