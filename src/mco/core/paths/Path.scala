package mco.core.paths

import scala.annotation.tailrec


/**
 * Class representing absolute paths in a pure fashion.
 *
 * Being a pure implementation, this class does not disambiguate
 * between "/" and "\" separator, as well as Windows or Linux
 * roots. It's suggested that interpreter provides a "virtual"
 * path on Windows at "/" containing FS roots.
 *
 * @param segments parts this path consists of, without ".", ".." and slashes
 */
sealed abstract case class Path (segments: Vector[Segment]) extends PathLike.Instance[Path] {
  def relTo(other: Path): RelPath = {
    @tailrec def loop(from: Vector[Segment], to: Vector[Segment]): Vector[Segment] =
      (from, to) match {
        case (a +: as, b +: bs) if a == b =>
          loop(as, bs)
        case (as, bs) =>
          Vector.fill(bs.size)(Segment.`..`) ++ as
      }
    RelPath(loop(segments, other.segments))
  }

  private def isWinLike = segments.headOption.exists(_.toString endsWith ":")

  override def toString: String = {
    if (isWinLike) segments.mkString("\\")
    else segments.mkString("/", "/", "")
  }

  override def withSegments(segments: Vector[Segment]): Path = Path.of(segments)
}

/**
 * Companion object to the Path class
 */
object Path extends PathLike.Companion[Path] {
  val root: Path = Path("/")
  def of(segments: Vector[Segment]): Path =
    new Path(Segment.multi(segments.mkString("/")).dropWhile(_ == Segment.`..`)) {}
}
