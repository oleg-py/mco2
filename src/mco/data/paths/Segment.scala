package mco.data.paths

sealed abstract case class Segment(override val toString: String) {
  def extension: String = {
    val idx = toString.lastIndexOf('.')
    if (idx == -1) "" else toString.drop(idx)
  }
}

object Segment {
  private val sepRx = """[\\/]+""".r

  def apply(s: String) = new Segment(sepRx.replaceAllIn(s, "")) {}

  val empty = Segment("")
  val `..` = Segment("..")

  // TODO - maybe single string in signature is enough
  def multi(segments: Seq[String]): Vector[Segment] = {
    (Vector.empty[Segment] /: segments.flatMap(sepRx.split)) {
      case (ss, "" | ".")          => ss
      case (v @ (`..` +: _), "..") => `..` +: v
      case (rest :+ _, "..")       => rest
      case (ss, next)              => ss :+ Segment(next)
    }
  }
}
