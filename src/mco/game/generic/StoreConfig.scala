package mco.game.generic

import scala.util.matching.Regex

import mco.core.paths.Segment


case class StoreConfig(
  files: StoreConfig.Files,
  repos: Map[String, StoreConfig.Repo]
)

object StoreConfig {
  case class Files(
    images: Regex
  ) {
    val isImageS = images.findFirstMatchIn(_: String).nonEmpty
    def isImage: Segment => Boolean = seg => isImageS(seg.toString)
  }

  case class Repo(
    title: String,
    mods: String,
    images: String,
    target: String
  ) {
    def paths: Vector[String] = Vector(mods, images, target)
  }
}
