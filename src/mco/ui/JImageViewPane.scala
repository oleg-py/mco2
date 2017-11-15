package mco.ui

import javafx.beans.property.SimpleObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.geometry.{HPos, VPos}
import javafx.scene.image.{ImageView => JImageView}
import javafx.scene.layout.{Region => JRegion}

class JImageViewPane(iv: JImageView) extends JRegion {
  val imageViewProperty = new SimpleObjectProperty[JImageView]()

  imageViewProperty.addListener(new ChangeListener[JImageView]() {
    override def changed(arg0: ObservableValue[_ <: JImageView] , oldIV: JImageView , newIV: JImageView): Unit = {
      Option(oldIV).foreach(getChildren.remove)
      Option(newIV).foreach(getChildren.add)
    }
  })

  imageViewProperty.set(iv)

  def this() = this(new JImageView())

  protected override def layoutChildren(): Unit = {
    for (imageView <- Option(imageViewProperty.get())) {
      imageView.setFitWidth(getWidth)
      imageView.setFitHeight(getHeight)
      layoutInArea(imageView, 0, 0, getWidth, getHeight, 0, HPos.CENTER, VPos.CENTER)
    }
    super.layoutChildren()
  }
}
