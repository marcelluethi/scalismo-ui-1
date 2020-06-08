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

package scalismo.ui.api

import java.awt.Color

import scalismo.geometry._3D
import scalismo.transformations.RigidTransformation
import scalismo.ui.control.interactor.landmark.complex.ComplexLandmarkingInteractor
import scalismo.ui.control.interactor.landmark.complex.posterior.PosteriorLandmarkingInteractor
import scalismo.ui.control.interactor.landmark.simple
import scalismo.ui.control.interactor.{DefaultInteractor, Interactor}
import scalismo.ui.model._
import scalismo.ui.model.properties.Uncertainty
import scalismo.ui.view.ScalismoFrame

sealed private[api] trait SimpleInteractor {
  type ConcreteInteractor <: Interactor

  def ui: ScalismoUI

  protected[api] def peer: ConcreteInteractor

  ui.frame.interactor = peer
  peer.onActivated(ui.frame)

}

case class SimplePosteriorLandmarkingInteractor(ui: ScalismoUI,
                                                modelGroup: Group,
                                                targetGroup: Group,
                                                color: Color = Color.YELLOW,
                                                lineWidth: Int = 3,
                                                opacity: Double = 1.0)
    extends SimpleInteractor {

  type ConcreteInteractor = PosteriorLandmarkingInteractor

  override protected[api] lazy val peer: PosteriorLandmarkingInteractor = new PosteriorLandmarkingInteractor {

    val meshView: TriangleMeshView = ui.find[TriangleMeshView](modelGroup, (_: TriangleMeshView) => true).get

    private val previewGroup = Group(ui.frame.scene.groups.add("__preview__", hidden = true))

    // we start by copying the shape model transformations of the modelGroup into the previewGroup
    modelGroup.peer.shapeModelTransformations.poseTransformation.map(p =>
      previewGroup.peer.shapeModelTransformations.addPoseTransformation(p.transformation)
    )
    modelGroup.peer.shapeModelTransformations.gaussianProcessTransformation.map(g =>
      previewGroup.peer.shapeModelTransformations.addGaussianProcessTransformation(g.transformation)
    )

    override val previewNode: TriangleMeshNode = ui.show(previewGroup, meshView.triangleMesh, "previewMesh").peer
    frame.sceneControl.nodeVisibility.setVisibility(previewNode, frame.perspective.viewports, show = false)
    previewNode.color.value = color
    previewNode.lineWidth.value = lineWidth
    previewNode.opacity.value = opacity
    previewNode.pickable.value = false

    override val targetUncertaintyGroup: GroupNode = Group(
      ui.frame.scene.groups.add("__target_preview__", hidden = true)
    ).peer

    override def sourceGpNode: TransformationNode[DiscreteLowRankGpPointTransformation] =
      modelGroup.peer.shapeModelTransformations.gaussianProcessTransformation.get

    override def targetGroupNode: GroupNode = targetGroup.peer

    override val previewGpNode: TransformationNode[DiscreteLowRankGpPointTransformation] =
      previewGroup.peer.shapeModelTransformations.gaussianProcessTransformation.get

    override def frame: ScalismoFrame = ui.frame

    override val inversePoseTransform: RigidTransformation[_3D] =
      modelGroup.peer.shapeModelTransformations.poseTransformation
        .map(_.transformation.inverse)
        .getOrElse(PointTransformation.RigidIdentity)

  }
}

case class SimpleLandmarkingInteractor(ui: ScalismoUI) extends SimpleInteractor {

  override type ConcreteInteractor = Instance

  private[api] class Instance(override val frame: ScalismoFrame)
      extends DefaultInteractor
      with ComplexLandmarkingInteractor[Instance] {}

  override protected[api] lazy val peer: Instance = new Instance(ui.frame)
}

/**
 * This landmarking interactor does not edit uncertainties of landmarks.
 */
case class OneClickLandmarkingInteractor(ui: ScalismoUI, uncertainty: Uncertainty = Uncertainty.DefaultUncertainty)
    extends SimpleInteractor {

  override type ConcreteInteractor = scalismo.ui.control.interactor.landmark.simple.SimpleLandmarkingInteractor.type

  override protected[api] lazy val peer: simple.SimpleLandmarkingInteractor.type =
    scalismo.ui.control.interactor.landmark.simple.SimpleLandmarkingInteractor

}
