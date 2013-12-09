package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{winWidth, winHeight, renderMode, RenderMode, Normal, Split, eyeCorrection}
import scala.collection.mutable
import java.io.File
import util.Random._

object Model {
  private[this] val rawcache = mutable.HashMap[String, RawModel]()

  type Vertices = Vector[Vec]
  type Faces = Vector[Array[(Int, Int, Int)]] //Array of indices vertex,tex,normal
  type UVVertices = Vector[UV]
  type Normals = Vector[Vec]

  object OBJModel {
    private[this] val vReg = "v (.*?) (.*?) (.*?)".r // vertices
    private[this] val fReg = "f (.*)".r              // faces
    private[this] val fcReg1 = "([0-9]+)/([0-9]+)".r           // face coord, vertex/uvvertex 
    private[this] val fcReg2 = "([0-9]+)//([0-9]+)".r          // face coord, vertex//normal
    private[this] val fcReg3 = "([0-9]+)/([0-9]+)/([0-9]+)".r  // face coord, vertex/uvvertex/normal
    
    private[this] val vnReg = "vn (.*?) (.*?) (.*?)".r // vertices
    private[this] val vtReg = "vt (.*?) (.*?)".r     // uv vertices

    def apply(fileStr: String): RawModel = rawcache.getOrElseUpdate(fileStr, {
      val file = io.Source.fromFile(fileStr)

      var vertices: Vertices = Vector.empty
      var faces: Faces = Vector.empty
      var uvVertices: UVVertices = Vector.empty
      var normals: Normals = Vector.empty

      file.getLines foreach {
        case vReg(x,y,z)  => vertices :+= Vec(x.toDouble, y.toDouble, z.toDouble)
        case vnReg(x,y,z) => normals :+= Vec(x.toDouble, y.toDouble, z.toDouble)
        case vtReg(u,v)   => uvVertices :+= UV(u.toDouble, v.toDouble)
        case fReg(f)      => faces :+= f.split(" ").map { 
          case fcReg1(v, vt)     => (v.toInt-1, vt.toInt-1, -1)
          case fcReg2(v, vn)     => (v.toInt-1,         -1, vn.toInt-1)
          case fcReg3(v, vt, vn) => (v.toInt-1, vt.toInt-1, vn.toInt-1)
          case v                 => (v.toInt-1,         -1, -1) }

        case _ => // nop
      }
      file.close

      RawModel(vertices, uvVertices, normals, faces)
    })

    def preload(files: Array[File]) {
      files
        .filterNot { file => rawcache.contains(file.toString) }
        .par.map { file => 
          val rawModel = apply(file.toString)
          rawcache(file.toString) = rawModel
          rawModel
        }
        .seq.foreach { rawModel => 
          rawModel.displayList
        }
    }
  }

  trait VecLike {
    def x: Double
    def y: Double
    def z: Double

    def +(v: VecLike) = Vec(x+v.x, y+v.y, z+v.z)
    def *(f: Double) = Vec(x*f, y*f, z*f)
    def /(f: Double) = Vec(x/f, y/f, z/f)
    def distance(v: Vec): Double = sqrt(pow(x-v.x, 2) + pow(y-v.y, 2) + pow(z-v.z, 2))
    def minCoord(v: VecLike) = Vec(min(x,v.x), min(y,v.y), min(z,v.z))
    def maxCoord(v: VecLike) = Vec(max(x,v.x), max(y,v.y), max(z,v.z))
    def span(v: VecLike) = Vec(abs(x-v.x), abs(y-v.y), abs(z-v.z))
  }
  object Vec {
    def random = Vec(nextDouble,nextDouble,nextDouble)
  }
  case class Vec(val x: Double, val y: Double, val z: Double) extends VecLike
  implicit def mutableVec(it: Vec): MutableVec = MutableVec(it.x,it.y,it.z)
  case class MutableVec(var x: Double, var y: Double, var z: Double) extends VecLike {
    def +=(v: VecLike): Unit = { x += v.x; y += v.y; z += v.z; }
    def *=(f: Double): Unit = { x *= f; y *= f; z *= f; }
  }
  final val Vec0 = Vec(0,0,0)
  final val Vec1 = Vec(1,1,1)

  trait TransformLike {
    def pos: Vec
    def rot: Vec
    def size: Vec
  }
  case class Transform(val pos: Vec = Vec0, val rot: Vec = Vec0, val size: Vec = Vec0) extends TransformLike
  implicit def mutableTransform(it: Transform): MutableTransform = MutableTransform(it.pos,it.rot,it.size) //meh
  case class MutableTransform(var pos: Vec = Vec0, var rot: Vec = Vec0, var size: Vec = Vec0) extends TransformLike {
    def +=(vector: TransformLike) {
      pos = pos + vector.pos
      rot = rot + vector.rot
      size = size + vector.size
    }
  }
  final val Transform001 = Transform(Vec0, Vec0, Vec1)
  final val Transform000 = Transform(Vec0, Vec0, Vec0)

  case class UV(u: Double, v: Double)

  case class Color(var r: Double, var g: Double, var b: Double) {
    def -=(f: Double) { r -= f; g -= f; b -= f; }
  }

  val cam = new Camera
  cam.setViewPort(0,0,winWidth,winHeight)
  cam.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  cam.setPerspective(90, (winWidth)/winHeight.toFloat, 1f, 500f)
  cam.setPosition(0,0,0);
  cam.lookAt(Vec3(0,0,200))
  
  case class RawModel(vertices: Vertices, uvVertices: UVVertices, normals: Normals, faces: Faces) {
    // Compile model to display list for faster drawing
    lazy val displayList = {
      val displayList = glGenLists(1)
      glNewList(displayList, GL_COMPILE)
      for(f <- faces) {
        glBegin(GL_POLYGON)
        for((vi, vti, vni) <- f) {
          if(vni != -1) glNormal3d(normals(vni).x, normals(vni).y, normals(vni).z)
          if(vti != -1) glTexCoord2d(uvVertices(vti).u, uvVertices(vti).v)
          glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
        }
        glEnd()
      }
      glEndList()

      displayList
    }

    def toModel( // TODO: Ouch, this sucks, but gets around mutability of the memoization cache
        transform: MutableTransform = Transform001,
        transformVector: MutableTransform = Transform000,
        oscillatorPhase: Double = 0,
        tex: Int = -1,
        color: Color = Color(0.5,0.5,0.5),
        alpha: Double = 1d) = Model(displayList, transform, transformVector, oscillatorPhase, tex, color, alpha)
  }

  case class Model(
      val displayList: Int,
      val transform: MutableTransform,
      val transformVector: MutableTransform,
      val oscillatorPhase: Double,
      val tex: Int,
      val color: Color,
      val alpha: Double) {

    def render(mode: RenderMode = renderMode, transform: MutableTransform = transform, tex: Int = tex, color: Color = color, alpha: Double = alpha) {
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
