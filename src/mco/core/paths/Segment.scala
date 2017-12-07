package mco.core.paths


sealed abstract case class Segment(override val toString: String) {
  def extension: String = {
    val idx = toString.lastIndexOf('.')
    if (idx == -1) "" else toString.drop(idx)
  }
}

object Segment {
  private val sepRx = """[\\/]+""".r

  def apply(s: String) = new Segment(sepRx.replaceAllIn(s, "").trim) {}

  val empty = Segment("")
  val `..` = Segment("..")

  def multi(s: String): Vector[Segment] = {
    (Vector.empty[Segment] /: sepRx.split(s)) {
      case (ss, "" | ".")          => ss
      case (v @ (`..` +: _), "..") => `..` +: v
      case (rest :+ _, "..")       => rest
      case (ss, next)              => ss :+ Segment(next)
    }
  }
  implicit val ordering: Ordering[Segment] = Ordering.by(_.toString)
}
