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

package scalismo.ui.model

import scalismo.geometry.{_3D, Point}

/**
 * Represents a bounding box of a 3D object.
 *
 * The special singleton BoundingBox.Invalid represents an invalid bounding box (e.g. when there are no objects in a viewport).
 *
 */
sealed trait BoundingBox {
  def xMin: Double

  def xMax: Double

  def yMin: Double

  def yMax: Double

  def zMin: Double

  def zMax: Double

  def union(that: BoundingBox): BoundingBox

  def contains(point: Point[_3D]): Boolean

  def center: Point[_3D]
}

object BoundingBox {

  case object Invalid extends BoundingBox {
    override def xMin = 0

    override def xMax = 0

    override def yMin = 0

    override def yMax = 0

    override def zMin = 0

    override def zMax = 0

    override def center: Point[_3D] = Point(0, 0, 0)

    override def contains(point: Point[_3D]): Boolean = false

    override def union(that: BoundingBox): BoundingBox = that
  }

  case class Valid private[BoundingBox] (xMin: Double,
                                         xMax: Double,
                                         yMin: Double,
                                         yMax: Double,
                                         zMin: Double,
                                         zMax: Double)
      extends BoundingBox {

    override def union(that: BoundingBox): BoundingBox = {
      if (that == Invalid) this
      else {
        BoundingBox(
          Math.min(this.xMin, that.xMin),
          Math.max(this.xMax, that.xMax),
          Math.min(this.yMin, that.yMin),
          Math.max(this.yMax, that.yMax),
          Math.min(this.zMin, that.zMin),
          Math.max(this.zMax, that.zMax)
        )
      }
    }

    override def contains(point: Point[_3D]): Boolean = {
      xMin <= point(0) && xMax >= point(0) && yMin <= point(1) && yMax >= point(1) && zMin <= point(2) && zMax >= point(
        2
      )
    }

    override def center: Point[_3D] = Point((xMin + xMax) / 2, (yMin + yMax) / 2, (zMin + zMax) / 2)

    override def toString: String = {
      s"BoundingBox ($xMin -> $xMax)($yMin -> $yMax)($zMin -> $zMax)"
    }
  }

  def apply(xMin: Double, xMax: Double, yMin: Double, yMax: Double, zMin: Double, zMax: Double): BoundingBox = {
    if (xMin > xMax || yMin > yMax || zMin > zMax) Invalid else Valid(xMin, xMax, yMin, yMax, zMin, zMax)
  }
}
