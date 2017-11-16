package mco.ui.components

import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.delegate.SFXDelegate
import scalafx.scene.layout.Region

import javafx.scene.image.{Image, ImageView}

/**
 * An `ImageView` that will attempt to fit the width or height of the window
 *
 * @see <a href="https://bugs.openjdk.java.net/browse/JDK-8091216">Relevant ticket in JDK bug system</a>
 */
class ImageViewPane (override val delegate: JImageViewPane = new JImageViewPane(new ImageView))
  extends Region(delegate)
    with SFXDelegate[JImageViewPane] {
  def image: ObjectProperty[Image] = delegate.imageViewProperty().imageProperty()
  def image_=(image: Image): Unit = delegate.imageViewProperty().setImage(image)

  def preserveRatio: BooleanProperty = delegate.imageViewProperty().preserveRatioProperty()
  def preserveRatio_=(value: Boolean): Unit = delegate.imageViewProperty().setPreserveRatio(value)

  def smooth: BooleanProperty = delegate.imageViewProperty().smoothProperty()
  def smooth_=(value: Boolean): Unit = delegate.imageViewProperty().setSmooth(value)
}
