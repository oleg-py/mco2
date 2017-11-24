package mco.data.paths

import scala.annotation.tailrec


/**
 * Class representing filesystem paths in a pure fashion.
 *
 * Being a pure implementation, this class does not disambiguate
 * between "/" and "\" separator, as well as Windows or Linux
 * roots. It's suggested that interpreter provides a "virtual"
 * path on Windows at "/" containing FS roots.
 *
 * @param segments parts this path consists of, without ".", ".." and slashes
 */
sealed abstract case class Path (segments: Vector[Segment]) {
  def name: Segment = segments.lastOption.getOrElse(Segment.empty)
  def extension: String = name.extension

//  @deprecated("method should be removed", "2.0.0-RC1") // TODO - inline to fromToP
  def fromTo(other: Path): Vector[Segment] = {
    @tailrec def loop(from: Vector[Segment], to: Vector[Segment]): Vector[Segment] =
      (from, to) match {
        case (a +: as, b +: bs) if a == b =>
          loop(as, bs)
        case (as, bs) =>
          Vector.fill(bs.size)(Segment.`..`) ++ as
      }

    loop(segments, other.segments)
  }

  def fromToP(other: Path): RelPath = RelPath(fromTo(other))
  def / (other: Segment) = Path.of(segments :+ other)
  def / (other: RelPath) = Path.of(segments ++ other.segments)

//  @deprecated("method should be removed", "2.0.0-RC1") // TODO - remove method
  def relStringTo(other: Path): String = fromTo(other).mkString("/")

  private def isWinLike = segments.headOption.exists(_.toString endsWith ":")

  override def toString: String = {
    if (isWinLike) segments.mkString("\\")
    else segments.mkString("/", "/", "")
  }
}

/**
 * Companion object to the Path class
 */
object Path {
  val root: Path = Path("/")
  def apply(str: String): Path = Path.of(Segment.multi(Seq(str)))
  def of(segments: Vector[Segment]): Path =
    new Path(Segment.multi(Seq(segments.mkString("/"))).dropWhile(_ == Segment.`..`)) {}
}
