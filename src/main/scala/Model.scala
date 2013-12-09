package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{winWidth, winHeight, renderMode, RenderMode, Normal, Split, eyeCorrection}
import scala.collection.mutable
import java.io.File

object Model {
  private[this] val cache = mutable.HashMap[String, RawModel]()

  object OBJModel {
    private[this] val vReg = "v (.*?) (.*?) (.*?)".r // vertices
    private[this] val fReg = "f (.*)".r              // faces
    private[this] val fcReg = "(.*?)/(.*?)".r        // face coord, vertex/uvvertex 
    
    private[this] val vtReg = "vt (.*?) (.*?)".r     // uv vertices

    def apply(fileStr: String): RawModel = cache.getOrElseUpdate(fileStr, {
      val file = io.Source.fromFile(fileStr)

      var vertices: Vertices = Vector.empty
      var faces: Faces = Vector.empty
      var uvVertices: UVVertices = Vector.empty
      file.getLines foreach {
        case vReg(x,y,z) => vertices :+= Vec(x.toDouble, y.toDouble, z.toDouble)
        case fReg(f)     => faces :+= f.split(" ").map { case fcReg(v, vt) => (v.toInt - 1, vt.toInt -1); case v => (v.toInt - 1, -1) }

        case vtReg(u,v)  => uvVertices :+= UV(u.toDouble, v.toDouble)

        case _ => // nop
      }
      file.close

      RawModel(vertices, faces, uvVertices)
    })

    def preload(files: Array[File]) {
      files
        .filterNot { file => cache.contains(file.toString) }
        .par.map { file => 
          val rawModel = apply(file.toString)
          cache(file.toString) = rawModel
          rawModel
        }
        .seq.foreach { rawModel => 
          rawModel.displayList
        }
    }
  }

  type Vertices = Vector[Vec]
  type Faces = Vector[Array[(Int, Int)]]
  type UVVertices = Vector[UV]
  
  case class Vec(var x: Double, var y: Double, var z: Double) {
    def +(v: Vec): Vec = Vec(x+v.x, y+v.y, z+v.z)
    def +=(v: Vec): Unit = { x += v.x; y += v.y; z += v.z; }
    def /(f: Double): Vec = Vec(x/f, y/f, z/f)
    def *(f: Double): Vec = Vec(x*f, y*f, z*f)
    def minCoord(v: Vec): Vec = Vec(min(x,v.x), min(y,v.y), min(z,v.z))
    def maxCoord(v: Vec): Vec = Vec(max(x,v.x), max(y,v.y), max(z,v.z))
    def span(v: Vec): Vec = Vec(abs(x-v.x), abs(y-v.y), abs(z-v.z))
  }
  final val Vec0 = Vec(0,0,0)
  final val Vec1 = Vec(1,1,1)

  case class Transform(var pos: Vec = Vec0, var rot: Vec = Vec0, var size: Vec = Vec1) {
    def +=(vector: Transform) {
      pos = pos + vector.pos
      rot = rot + vector.rot
      size = size + vector.size
    }
  }
  final val Transform001 = Transform(Vec0, Vec0, Vec1)
  final val Transform000 = Transform(Vec0, Vec0, Vec0)

  case class UV(u: Double, v: Double)

  case class Color(r: Double, g: Double, b: Double)
  final val white = Color(0,0,0)
  final val gray = Color(0.5,0.5,0.5)
  final val black = Color(0,0,0)

  //case class Color(r: Float, g: Float, b: Float, a: Float = 1)
  //final val white = Color(1,1,1)
  //final val black = Color(0,0,0)

  val cam = new Camera
  cam.setViewPort(0,0,winWidth,winHeight)
  cam.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  cam.setPerspective(90, (winWidth)/winHeight.toFloat, 1f, 1600f)
  cam.setPosition(0,0,-20);
  
  case class RawModel(vertices: Vertices, faces: Faces, uvVertices: UVVertices) {
    // Compile model to display list for faster drawing
    lazy val displayList = {
      val displayList = glGenLists(1)
      glNewList(displayList, GL_COMPILE)
      for(f <- faces) {
        glBegin(GL_POLYGON)
        for((vi, vti) <- f) {
          if(vti != -1) glTexCoord2d(uvVertices(vti).u, uvVertices(vti).v)
          glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
        }
        glEnd()
      }
      glEndList()

      displayList
    }

    def toModel( // TODO: Ouch, this sucks, but gets around mutability of the memoization cache
        transform: Transform = Transform001,
        transformVector: Transform = Transform000,
        tex: Int = -1,
        color: Color = gray,
        alpha: Double = 1d) = Model(displayList, transform, transformVector, tex, color, alpha)
  }

  case class Model(
      /*vertices: Vertices,
      faces: Faces,
      uvVertices: UVVertices,*/
      var displayList: Int,
      var transform: Transform = Transform001,
      var transformVector: Transform = Transform000,
      var tex: Int = -1,
      var color: Color = gray,
      var alpha: Double = 1d) {

    def applyTransformVector() {
      transform += transformVector
    }

    def render(mode: RenderMode = renderMode, transform: Transform = transform, tex: Int = tex, color: Color = color, alpha: Double = alpha) {
      def render0() {
        import transform._
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)
        
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        
        if(tex != -1) {
          glEnable(GL_TEXTURE_2D)
          glBindTexture(GL_TEXTURE_2D, tex)
        } else {
          glDisable(GL_TEXTURE_2D)
        }

        glPushMatrix()
          glTranslated(pos.x, pos.y, pos.z)
          glRotated(rot.x, 1,0,0)
          glRotated(rot.y, 0,1,0)
          glRotated(rot.z, 0,0,1)
          glScaled(size.x, size.y, size.z)
          glColor4d(color.r,color.g,color.b,alpha)
          glCallList(displayList)
        glPopMatrix()

        glDisable(GL_TEXTURE_2D)

        glDisable(GL_BLEND)

        glEnable(GL_LIGHTING)
        glEnable(GL_DEPTH_TEST)
      }

      mode match {
        case Normal() => 
          //cam.render
          render0
        case Split() =>
          val x = 15
          //cam.render
          glEnable(GL_SCISSOR_TEST)
          glPushMatrix
            glScissor(0,0,winWidth/2, winHeight);
            glTranslated(-winWidth/4-eyeCorrection,0,0)
            cam.pos.x -= x
            cam.lookAtV.x -= x
            cam.render
            cam.lookAtV.x += x
            cam.pos.x += x
            render0
          glPopMatrix
          glPushMatrix
            glScissor(winWidth/2,0,winWidth/2, winHeight);
            glTranslated(winWidth/4+eyeCorrection,0,0)
            cam.lookAtV.x += x
            cam.pos.x += x
            cam.render
            cam.pos.x -= x
            cam.lookAtV.x -= x
            render0
          glPopMatrix
          glDisable(GL_SCISSOR_TEST)
      }
    }
  }
}
