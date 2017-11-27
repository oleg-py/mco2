package mco

import scalaz._

import mco.data.paths._
import mco.core.Capture.yolo._
import mco.stubs.{ImmutableVar, VarFilesystem}
import mco.stubs.cells._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalatest._
import org.scalatest.enablers.{Emptiness, Existence}
import org.scalatest.prop.GeneratorDrivenPropertyChecks


object Tests {
  trait Sync extends FlatSpec with Matchers {
    def immutableFs(contents: (Segment, Cell)*) =
      new VarFilesystem(new ImmutableVar(dir(contents: _*)))
  }

  trait Prop extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

    implicit val arbitrarySegment: Arbitrary[Segment] =
      Arbitrary(arb[String].map(Segment.apply))

    implicit val arbitraryPath: Arbitrary[Path] =
      Arbitrary(arb[Vector[Segment]].map(Path.of))

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
      def asFile = File(p.toString)
    }

    implicit class BetterFileToMcoPath(f: File) {
      def asPath = Path(f.pathAsString)
    }
  }
}
