/*
 * Copyright (C) 2016  University of Basel, Graphics and Vision Research Group
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package scalismo.ui.rendering.actor.mixin

import scalismo.ui.model.properties.{NodeProperty, ScalarRangeProperty}
import scalismo.ui.rendering.actor.{ActorEvents, SingleDataSetActor}

trait ActorScalarRange extends SingleDataSetActor with ActorEvents {
  def scalarRange: ScalarRangeProperty

  listenTo(scalarRange)

  reactions += {
    case NodeProperty.event.PropertyChanged(p) if p eq scalarRange => setAppearance()
  }

  private def setAppearance(): Unit = {
    val range = scalarRange.value

    mapper.SetScalarRange(range.mappedMinimum, range.mappedMaximum)

    val lowerValue = range.mappedMinimum
    val upperValue = range.mappedMaximum
    val colorMappingFunction = range.colorMapping.mappingFunction(range)

    val colorTransferFun = new vtk.vtkColorTransferFunction()
    colorTransferFun.SetRange(lowerValue, upperValue)
    colorTransferFun.SetScaleToLinear()
    colorTransferFun.SetColorSpaceToRGB()
    val step: Double = (upperValue - lowerValue) / range.colorMapping.suggestedNumberOfColors
    for (i <- 0 until range.colorMapping.suggestedNumberOfColors) {
      val value = lowerValue + i * step
      val color = colorMappingFunction(value)
      colorTransferFun.AddRGBPoint(value, color.getRed / 255.0, color.getGreen / 255.0, color.getBlue / 255.0)
    }

    mapper.SetLookupTable(colorTransferFun)
    mapper.Modified()
    actorChanged()
  }

  setAppearance()
}
