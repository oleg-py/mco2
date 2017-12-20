package mco.game.generic


case class StoreConfig(
  files: StoreConfig.Files,
  repos: Map[String, StoreConfig.Repo]
)

object StoreConfig {
  case class Files(
    images: Vector[String]
  ) {
    def isImagePath(s: String): Boolean = images.exists(s.endsWith)
  }

  case class Repo(
    title: String,
    mods: String,
    images: String,
    target: String
  )
}
