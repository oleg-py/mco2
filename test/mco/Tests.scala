package mco

import mco.core.paths._
import mco.stubs.{ConstantVar, VarFilesystem}
import mco.stubs.cells._
import monix.eval.Coeval
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalatest._
import org.scalatest.enablers.{Emptiness, Existence}
import org.scalatest.prop.GeneratorDrivenPropertyChecks


object Tests {
  trait Sync extends FlatSpec with Matchers {
    def immutableFs(contents: (Segment, Cell)*) =
      new VarFilesystem[Coeval](new ConstantVar(dir(contents: _*)))
  }

  trait Prop extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

    implicit val arbitrarySegment: Arbitrary[Segment] =
      Arbitrary(arb[String].map(Segment.apply))

    implicit val arbitraryPath: Arbitrary[Path] =
      Arbitrary(arb[Vector[Segment]].map(Path.of))
  }

  trait SyncFixture extends fixture.FlatSpec with Matchers

  trait BetterFilesHelpers {
    import better.files._
    implicit val betterFileExistence: Existence[File] = _.exists
    implicit val mcoPathExistence : Existence[Path] = _.asFile.exists
    implicit val mcoPathCExistence : Existence[Coeval[Path]] = _.asFile.exists
    implicit val betterFileEmptiness: Emptiness[File] = _.isEmpty
    implicit val mcoPathEmptiness: Emptiness[Path] = _.asFile.isEmpty
    implicit val mcoPathCEmptiness: Emptiness[Coeval[Path]] = _.asFile.isEmpty

    implicit class McoPathToBetterFiles(p: Path) {
      def asFile = File(p.toString)
    }

    implicit class McoPathCoevalToBetterFiles(p: Coeval[Path]) {
      def asFile = File(p().toString)
    }

    implicit class BetterFileToMcoPath(f: File) {
      def asPath = Path(f.pathAsString)
    }
  }
}
