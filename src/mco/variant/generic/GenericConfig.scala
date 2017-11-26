package mco.variant.generic

import scala.util.matching.Regex

import mco.data.paths.Segment


case class GenericConfig(
  files: GenericConfig.Files,
  repo: GenericConfig.Repo
)

object GenericConfig {
  case class Files(
    images: Regex
  ) {
    private val matchImg = images.findFirstMatchIn(_: String).nonEmpty
    def isImage: Segment => Boolean = seg => matchImg(seg.toString)
  }

  case class Repo(
    mods: String,
    images: String,
    target: String
  ) {
    def paths: Vector[String] = Vector(mods, images, target)
  }
}
