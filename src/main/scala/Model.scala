package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{winWidth, winHeight, renderMode, RenderMode, Normal, Split, eyeCorrection}
import scala.collection.mutable
import java.io.File
import util.Random._
import scala.language.implicitConversions

object Model {
  private[this] val rawcache = mutable.HashMap[String, RawModel]()

  type Vertices = Vector[Vec]
  type Faces = Vector[Array[(Int, Int, Int)]] //Array of indices vertex,tex,normal
  type UVVertices = Vector[UV]
  type Normals = Vector[Vec]

  object OBJModel {
    private[this] val fcReg1 = "([0-9]+)/([0-9]+)".r           // face coord, vertex/uvvertex 
    private[this] val fcReg2 = "([0-9]+)//([0-9]+)".r          // face coord, vertex//normal
    private[this] val fcReg3 = "([0-9]+)/([0-9]+)/([0-9]+)".r  // face coord, vertex/uvvertex/normal

    def apply(fileStr: String, lines: Boolean = false): RawModel = rawcache.getOrElseUpdate(fileStr, {
      val file = io.Source.fromFile(fileStr)

      var vertices: Vertices = Vector.empty
      var faces: Faces = Vector.empty
      var uvVertices: UVVertices = Vector.empty
      var normals: Normals = Vector.empty

      file.getLines foreach { line => 
        val x = line.split(" ")

        x(0) match {
          case "v"  => vertices :+= Vec(x(1).toDouble, x(2).toDouble, x(3).toDouble)
          case "vn" => normals :+= Vec(x(1).toDouble, x(2).toDouble, x(3).toDouble)
          case "vt" => uvVertices :+= UV(x(1).toDouble, x(2).toDouble)
          case "f"  => faces :+= x.tail.map {
              case fcReg1(v, vt)     => (v.toInt-1, vt.toInt-1, -1)
              case fcReg2(v, vn)     => (v.toInt-1,         -1, vn.toInt-1)
              case fcReg3(v, vt, vn) => (v.toInt-1, vt.toInt-1, vn.toInt-1)
              case v                 => (v.toInt-1,         -1, -1)
            }
          case _    => // nop
        }
      }

      file.close//*/

    /*private[this] val vReg = "v (.*?) (.*?) (.*?)".r // vertices
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
      file.close*/

      println("bump")

      RawModel(vertices, uvVertices, normals, faces, lines)
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
    def -(v: VecLike) = Vec(x-v.x, y-v.y, z-v.z)
    def unary_-() = Vec(-x, -y, -z)
    def *(f: Double) = Vec(x*f, y*f, z*f)
    def /(f: Double) = Vec(x/f, y/f, z/f)
    def distance(v: Vec): Double = sqrt(pow(x-v.x, 2) + pow(y-v.y, 2) + pow(z-v.z, 2))
    def minCoord(v: VecLike) = Vec(min(x,v.x), min(y,v.y), min(z,v.z))
    def maxCoord(v: VecLike) = Vec(max(x,v.x), max(y,v.y), max(z,v.z))
    def span(v: VecLike) = Vec(abs(x-v.x), abs(y-v.y), abs(z-v.z))
    def avg() = (x+y+z)/3d
    def setX(d: Double) = { Vec(d,y,z) }
    def setY(d: Double) = { Vec(x,d,z) }
    def setZ(d: Double) = { Vec(x,y,d) }
    def normalize() = {
      val m = max(max(abs(x),abs(y)),abs(z))
      Vec(x/m, y/m, z/m)
    }
  }
  object Vec {
    def random = random01
    def random01 = Vec(nextDouble,nextDouble,nextDouble)
    def random11 = Vec(nextGaussian,nextGaussian,nextGaussian)
    def randomUniform = { val r = nextDouble; Vec(r,r,r) } 
  }
  case class Vec(val x: Double, val y: Double, val z: Double) extends VecLike
  implicit def mutableVec(it: Vec): MutableVec = MutableVec(it.x,it.y,it.z)
  case class MutableVec(var x: Double, var y: Double, var z: Double) extends VecLike {
    //def +=(v: VecLike): Unit = { x += v.x; y += v.y; z += v.z; }
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
  implicit def imutableTransform(mt: MutableTransform): Transform = Transform(mt.pos,mt.rot,mt.size) //meh
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
    def *=(f: Double) { r *= f; g *= f; b *= f; }
  }
  object Color {
    def apply(d: Double): Color = Color(d,d,d)
  }

  val cam = new Camera
  cam.setViewPort(0,0,winWidth,winHeight)
  cam.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  cam.setPerspective(50, (winWidth)/winHeight.toFloat, 0.01f, 500f)
  cam.setPosition(0,0,0);
  cam.lookAt(Vec3(0,0,200))
  
  case class RawModel(vertices: Vertices, uvVertices: UVVertices, normals: Normals, faces: Faces, lines: Boolean = false) {
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
      if(lines) for(f <- faces) {
        glColor4d(0,0,0,1)
        glBegin(GL_LINE_STRIP)
        for((vi, vti, vni) <- f) {
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
        tex: Int = -1,
        color: Color,
        alpha: Double = 1d,
        phi: Double = 0,
        theta: Double = 0) = Model(displayList, transform, transformVector, tex, color, alpha, phi, theta)
  }

  case class Model(
      val displayList: Int,
      val transform: MutableTransform,
      val transformVector: MutableTransform,
      val tex: Int,
      val color: Color,
      val alpha: Double,
      var phi: Double,
      var theta: Double,
      var trail: Trail = new Trail()) {

    def render(mode: RenderMode = renderMode, transform: MutableTransform = transform, tex: Int = tex, color: Color = color, alpha: Double = alpha) {
      renderDis {
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
    }
  }


  /*case class Particle(transform: MutableTransform, transformVector: MutableTransform, color: Color, var dead: Boolean = false) {
    val spread = 3d
    transform.pos += (Vec.random * ((nextDouble-nextDouble)*spread))
    val spread2 = 20d
    //transformVector.pos += (Vec.random * ((nextDouble-nextDouble)*spread2))

    def render() {
      if(!dead) {
        transformVector.pos *= (0.9 - nextDouble*0.2)
        color *= (1.1 + nextDouble*0.2)
        val pos0 = transform.pos.copy()
        transform += transformVector
        if(color.r >= 1) dead = true


        glColor3d(color.r, color.g, color.b)
        import transform._
        //glBegin(GL_POINTS)
        val size = 0.1
        glBegin(GL_QUADS)
          glVertex3d(pos0.x, pos0.y, pos0.z)
          glVertex3d(pos.x, pos.y, pos.z)
          glVertex3d(pos.x+size, pos.y+size, pos.z+size)
          glVertex3d(pos0.x+size, pos0.y+size, pos0.z+size)
        glEnd()
      }
    }
  }*/

  class Trail(var points: Vector[Vec] = Vector.empty) {
    def +>=(t: Vec) { points :+= t }

    def render() {
      renderDis {
        val take = 100
        val ratio = 1/take.toDouble
        val error = Vec.random11 * 0.2
        points = points.takeRight(take)
        glEnable(GL_BLEND)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)
        glBegin(GL_LINE_STRIP)
        var i = take.toDouble
        for(v <- points.reverse) {
          val vec = v //+ (error * (take-i))
          glColor4d(0,0,0,i/take)
          glVertex3d(vec.x, vec.y, vec.z)
          //glVertex3d(vec.x+1, vec.y+1, vec.z+1)
          i -= 1
        }
        glEnd()
        glDisable(GL_BLEND)
        glDisable(GL_DEPTH_TEST)
        glDisable(GL_LIGHTING)
      }
    }
  }


  // See dis? I need dis rendered
  def renderDis(render: => Unit) {
    renderMode match {
      case Normal() => 
        render
      case Split() =>
        val x = 0.5f//Liminoid.testNum
        val x2 = 126+30+22//+Liminoid.testNum
        glEnable(GL_SCISSOR_TEST)
        glPushMatrix
          glScissor(0,0,winWidth/2, winHeight);
          cam.look(Vec3(-x,0,0), Vec3(-x2,0,0))
          render
        glPopMatrix
        glPushMatrix
          glScissor(winWidth/2,0,winWidth/2, winHeight);
          cam.look(Vec3(x,0,0), Vec3(x2,0,0))
          render
        glPopMatrix
        glDisable(GL_SCISSOR_TEST)
    }
  }
  
  
}
