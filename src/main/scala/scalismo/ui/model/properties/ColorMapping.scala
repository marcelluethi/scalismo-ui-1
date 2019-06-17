package scalismo.ui.model.properties

import java.awt.Color

import scalismo.color.RGB

/** Maps a range of values, determined by a lower and upper value to a color */
trait ColorMapping {

  def lowerColor: Color

  def upperColor: Color

  def mappingFunction(scalarRange: ScalarRange): (Double => Color)

  def suggestedNumberOfColors : Int
}

private[properties] case class LinearColorMapping(lowerColor : Color, upperColor : Color) extends ColorMapping {

  override def mappingFunction(scalarRange: ScalarRange): (Double => Color) = {
    value => {

      val lowerValue = scalarRange.cappedMinimum
      val upperValue = scalarRange.cappedMaximum
      if (value < lowerValue) lowerColor
      else if (value > upperValue) upperColor
      else {
        // interpolating color
        val s = (value - lowerValue) / (upperValue - lowerValue)
        val newColor = (RGB(upperColor) - RGB(lowerColor)) * s + RGB(lowerColor)
        newColor.toAWTColor
      }
    }
  }

  override val suggestedNumberOfColors = 100

}

object BlueToRedColorMapping extends LinearColorMapping(Color.BLUE, Color.GREEN) {

  override val suggestedNumberOfColors = 100

}


