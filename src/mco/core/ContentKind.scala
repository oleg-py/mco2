package mco.core


sealed trait ContentKind extends Product with Serializable

object ContentKind {
  def parse(s: String): Option[ContentKind] = s match {
    case "Document"  => Some(Document)
    case "Component" => Some(Component)
    case "Unused"    => Some(Unused)
    case _           => None
  }

  def stringify(ck: ContentKind): String = ck match {
    case Component => "Component"
    case Document  => "Document"
    case Unused    => "Unused"
  }

  case object Document   extends ContentKind
  case object Component  extends ContentKind
  case object Unused     extends ContentKind
}
