package mco

import scalaz._
import scalaz.std.option._
import scalaz.std.option.optionSyntax._
import scalaz.syntax.id._
import scalaz.syntax.std.map._

import mco.data.Path
import org.scalatest.Assertions


package object stubs {
  import Cell._
  type FState[A] = State[Dir, A]
  def complainAbout(path: Path) = Assertions.fail(s"Illegal access: $path")

  def deepGet(path: Path): FState[Option[Cell]] =
    for (root <- State.get[Dir]) yield
      path.segments.foldLeft(some[Cell](root)) {
        case (Some(Dir(cc)), key) => cc.get(key)
        case _ => None
      }

  def deepSet(path: Path)(obj: Option[Cell]): FState[Unit] = {
    def recurse(segments: List[String])(parent: Dir): Dir = {
      val Dir(cs) = parent

      segments match {
        case Nil       => complainAbout(path)
        case s :: Nil  => obj.cata(cs.updated(s, _), cs - s) |> Dir
        case s :: more => cs.alter(s) {
          case Some(_: File) => complainAbout(path)
          case Some(d: Dir)  => recurse(more)(d).some
          case None          => recurse(more)(Dir()).some
        } |> Dir
      }
    }

    State.modify(recurse(path.segments.toList))
  }

  def check(pf: PartialFunction[Option[Cell], Unit]) = (path: Path) =>
    deepGet(path) map { x =>
      if (!pf.isDefinedAt(x)) complainAbout(path)
    }

  val notFolder = check { case None | Some(File(_)) => }
  val notFile   = check { case None | Some(Dir(_)) => }
  val mustExist = check { case Some(_) => }
}
