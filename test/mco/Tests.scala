package mco

import scalaz._
import Id._

import mco.data.{Key, Path}
import mco.stubs.{Cell, ImmutableVar, VarFilesystem}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalatest._
import org.scalatest.enablers.{Emptiness, Existence}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object Tests {
  trait Sync extends FlatSpec with Matchers {
    implicit class Helpers(sc: StringContext) {
      def p(parts: Any*) = Path(sc.s(parts: _*))
      def key(parts: Any*) = Key(sc.s(parts: _*))
    }

    def immutableFs(contents: (String, Cell)*) =
      new VarFilesystem(new ImmutableVar(Cell.dir(contents: _*)))
  }

  trait Prop extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

    implicit val arbitraryPath: Arbitrary[Path] =
      Arbitrary(arb[List[String]].map(Path.of))

    implicit def arbitraryIMap[A: Order, B](
      implicit amap: Arbitrary[Map[A, B]]
    ): Arbitrary[IMap[A, B]] =
      Arbitrary(arb[Map[A, B]].map(_.toList).map(IMap.fromList(_)))
  }

  trait SyncFixture extends fixture.FlatSpec with Matchers

  trait BetterFilesHelpers {
    import better.files._
    implicit val betterFileExistence: Existence[File] = _.exists
    implicit val mcoPathExistence : Existence[Path] = _.asFile.exists
    implicit val betterFileEmptiness: Emptiness[File] = _.isEmpty
    implicit val mcoPathEmptiness: Emptiness[Path] = _.asFile.isEmpty

    implicit class McoPathToBetterFiles(p: Path) {
      def asFile = File(p.asString)
    }

    implicit class BetterFileToMcoPath(f: File) {
      def asPath = Path(f.pathAsString)
    }
  }
}
