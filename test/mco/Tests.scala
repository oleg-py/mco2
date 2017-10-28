package mco

import mco.data.Path
import org.scalatest._
import org.scalatest.enablers.{Emptiness, Existence}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object Tests {
  trait Sync extends FlatSpec with Matchers

  trait Prop extends PropSpec with Matchers with GeneratorDrivenPropertyChecks

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
