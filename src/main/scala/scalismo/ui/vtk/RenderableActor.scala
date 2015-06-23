package scalismo.ui.vtk

import scalismo.ui.Mesh.{ MeshRenderable2DOutline, MeshRenderable3D }
import scalismo.ui.PointCloud.PointCloudRenderable3D
import scalismo.ui.ScalarField.{ ScalarFieldRenderable3D }
import scalismo.ui.ScalarMeshField.ScalarMeshFieldRenderable3D
import scalismo.ui.VectorField.VectorFieldRenderable3D
import scalismo.ui.visualization.{ EllipsoidLike, Renderable }
import scalismo.ui.{ Image3D, BoundingBox, Scene }
import vtk.vtkActor

import scala.util.Try

object RenderableActor {
  type RenderableToActor = (Renderable, VtkViewport) => Option[RenderableActor]

  def apply(renderable: Renderable)(implicit vtkViewport: VtkViewport): Option[RenderableActor] = {
    // first, use the function in case the user overwrote something
    val raOption = renderableToActorFunction(renderable, vtkViewport)
    if (raOption.isDefined) raOption
    else {
      renderable match {
        case r: VtkRenderable => Some(r.getVtkActor)
        case _ =>
          println("RenderableActor: Dunno what to do with " + renderable.getClass)
          None
      }
    }
  }

  val DefaultRenderableToActorFunction: RenderableToActor = {
    case (renderable, vtkViewport) =>
      implicit val _vtkViewport = vtkViewport
      renderable match {
        case bb3d: Scene.SlicingPosition.BoundingBoxRenderable3D => Some(new BoundingBoxActor3D(bb3d))
        case sp3d: Scene.SlicingPosition.SlicingPlaneRenderable3D => Some(new SlicingPlaneActor3D(sp3d))
        case sp2d: Scene.SlicingPosition.SlicingPlaneRenderable2D => Some(new SlicingPlaneActor2D(sp2d))
        case m3d: MeshRenderable3D => Some(new MeshActor3D(m3d))
        case smf3d: ScalarMeshFieldRenderable3D => Some(new ScalarMeshFieldActor(smf3d))
        case sf3d: ScalarFieldRenderable3D => Some(new ScalarFieldActor3D(sf3d))
        case pc3d: PointCloudRenderable3D => Some(new PointCloudActor3D(pc3d))
        case vf3d: VectorFieldRenderable3D => Some(new VectorFieldActor3D(vf3d))
        case m2d: MeshRenderable2DOutline => Some(new MeshActor2DOutline(m2d))
        case img3d: Image3D.Renderable3D[_] => img3d.imageOrNone.map {
          source => new ImageActor3D(source)
        }
        case img2d: Image3D.Renderable2D[_] => img2d.imageOrNone.map {
          source => ImageActor2D(source)
        }
        case ell: EllipsoidLike => Some(EllipsoidActor.apply(vtkViewport, ell))
        //        case s: EllipsoidLike => s.dim match {
        //          case 3 => Some (new EllipsoidActor3D (s) )
        //          case 2 => Some (new EllipsoidActor2D (s) )
        //          case _ => None
        //        }
        case _ => None
      }
  }

  private var _renderableToActorFunction = DefaultRenderableToActorFunction

  def renderableToActorFunction = this.synchronized(_renderableToActorFunction)

  def renderableToActorFunction(nf: RenderableToActor) = this.synchronized {
    _renderableToActorFunction = nf
  }
}

trait RenderableActor extends VtkContext {
  def vtkActors: Seq[vtkActor]

  def currentBoundingBox: BoundingBox

  def onDestroy(): Unit = {
    vtkActors.foreach {
      a =>
        Try {
          a.Delete()
        }
    }
  }
}