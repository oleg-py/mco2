package mco.variant.generic

case class GenericConfig(
  mods: String,
  images: String,
  target: String
) {
  def paths: Vector[String] = Vector(mods, images, target)
}
