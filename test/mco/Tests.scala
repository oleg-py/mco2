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
  trait Spec extends FlatSpec
    with Matchers
    with TestUtils

  trait Props extends PropSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with TestUtils

  trait SpecFixture extends fixture.FlatSpec
    with Matchers
    with TestUtils

  trait TestUtils extends FsUtils
    with Arbitraries
    with BetterFilesHelpers

  trait FsUtils {
    def immutableFs(contents: (Segment, Cell)*) =
      new VarFilesystem[Coeval](new ConstantVar(dir(contents: _*)))
  }

  trait Arbitraries {
    implicit val arbitrarySegment: Arbitrary[Segment] =
      Arbitrary(arb[String].map(Segment.apply))

    implicit val arbitraryPath: Arbitrary[Path] =
      Arbitrary(arb[Vector[Segment]].map(Path.of))
  }

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
