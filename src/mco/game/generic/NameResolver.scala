package mco.game.generic

import enumeratum._
import mco.core.paths._
import net.openhft.hashing.LongHashFunction
import pureconfig.ConfigConvert

import java.util.UUID


trait NameResolver {
  def apply(mod: RelPath, content: RelPath): Path
}

object NameResolver {
  sealed abstract class Factory(val make: Path => NameResolver)
    extends EnumEntry

  object Factory extends Enum[Factory] {
    case object Subdirs extends Factory(target => (parent, content) => {
      path"$target/$parent/$content"
    })

    case object Overrides extends Factory(target => (_, content) => {
      path"$target/$content"
    })


    case object Flatten extends Factory(target => (_, content) => {
      target / content.name
    })

    case object Mangle extends Factory(target => (mod, content) => {
      val hash = LongHashFunction.xx()
      val uuid = new UUID(hash.hashChars(mod.toString), hash.hashChars(content.toString))
      val filename = seg"${uuid.toString ++ content.extension}"
      target / filename
    })

    implicit val configReader = ConfigConvert.viaStringOpt[Factory](
      Factory.withNameInsensitiveOption,
      _.entryName.toLowerCase
    )

    override def values = findValues
  }

}