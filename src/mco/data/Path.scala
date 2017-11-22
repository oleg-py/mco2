package mco.data

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
final class Path private (val segments: Vector[String]) {
  def name: String = segments.lastOption.getOrElse("")
  def extension: String = name.dropWhile(_ == '.').dropWhile(_ != '.')

  def / (s: String): Path = this / Seq(s)
  def / (ss: Seq[String]): Path = Path.of(segments ++ ss)

  def relTo(other: Path): Vector[String] = {
    @tailrec def loop(from: Vector[String], to: Vector[String]): Vector[String] =
      (from, to) match {
        case (a +: as, b +: bs) if a == b =>
          loop(as, bs)
        case (as, bs) =>
          Vector.fill(bs.size)("..") ++ as
      }

    loop(segments, other.segments)
  }

  def relStringTo(other: Path): String = relTo(other).mkString("/")

  private def isWinLike = segments.headOption.exists(_ endsWith ":")

  def asString: String = {
    if (isWinLike) segments.mkString("\\")
    else segments.mkString("/", "/", "")
  }

  override def equals(other: Any): Boolean = other match {
    case p: Path => segments == p.segments
    case _       => false
  }

  override def hashCode(): Int = segments.##
  override def toString = s"Path($asString)"
}

/**
 * Companion object to the Path class
 */
object Path {
  private val sepRx = """[\\/]+""".r

  val root: Path = Path("/")
  def apply(str: String): Path = of(Seq(str))
  def segment(str: String): String = sepRx.replaceAllIn(str, "-")

  def of(segments: Seq[String]): Path = new Path(normalize(segments))

  def normalize(segments: Seq[String]): Vector[String] = {
    (Vector.empty[String] /: segments.flatMap(sepRx.split)) {
      case (ss, "" | ".")        => ss
      case (ss @ Vector(), "..") => ss
      case (rest :+ _, "..")     => rest
      case (ss, next)            => ss :+ next
    }
  }
}
