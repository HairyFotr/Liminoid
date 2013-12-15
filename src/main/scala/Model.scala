package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{winWidth, winHeight, renderMode, RenderMode, Normal, Stereo, eyeCorrection}
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
    def apply(fileStr: String): RawModel = rawcache.getOrElseUpdate(fileStr, {
      val file = io.Source.fromFile(fileStr)

      var vertices: Vertices = Vector.empty
      var faces: Faces = Vector.empty
      var normals: Normals = Vector.empty
      var uvVertices: UVVertices = Vector.empty

      file.getLines.buffered foreach { line =>
        val x = line.split(" ")
        
        x(0) match {
          case "v"  => vertices :+= Vec(x(1).toDouble, x(2).toDouble, x(3).toDouble)
          case "vn" => normals :+= Vec(x(1).toDouble, x(2).toDouble, x(3).toDouble)
          case "f"  => faces :+= (x.tail.map { face =>
              val fs = face.split("/")
              if(fs.length == 1)      (fs(0).toInt-1,            -1, -1)
              else if(fs.length == 2) (fs(0).toInt-1, fs(1).toInt-1, -1)
              else if(fs(1).isEmpty)  (fs(0).toInt-1,            -1, fs(2).toInt-1)
              else                    (fs(0).toInt-1, fs(1).toInt-1, fs(2).toInt-1)
            })

          case "vt" => uvVertices :+= UV(x(1).toDouble, x(2).toDouble)
          case _    => // nop
        }
      }

      file.close

      new RawModel(vertices, uvVertices, normals, faces)
    })

    def preload(files: Array[File], max: Int = -1) {
      (if(max == -1) files else files.take(max))
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
    def zeroX() = { Vec(0,y,z) }
    def zeroY() = { Vec(x,0,z) }
    def zeroZ() = { Vec(x,y,0) }
    def normalize() = {
      val m = max(max(abs(x),abs(y)),abs(z))
      Vec(x/m, y/m, z/m)
    }
  }
  object Vec {
    def random = random01
    def random360 = random01 * 360
    def random01 = Vec(nextDouble,nextDouble,nextDouble)
    def random11 = Vec(nextGaussian,nextGaussian,nextGaussian)
    def randomUniform01 = { val r = nextDouble; Vec(r,r,r) } 
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
    def RGB(i: Int): Color = Color(((i & 255)/255d), (((i >> 8) & 255)/255d), (((i >> 16) & 255)/255d))
    def BGR(i: Int): Color = Color((((i >> 16) & 255)/256d), (((i >> 8) & 255)/256d), ((i & 255)/256d))
  }

  val cam = new Camera
  cam.setViewPort(0,0,winWidth,winHeight)
  cam.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  cam.setPerspective(50, (winWidth)/winHeight.toFloat, 0.01f, 500f)
  cam.setPosition(0,0,0);
  cam.lookAt(Vec3(0,0,200))
  
  class RawModel(var vertices: Vertices, var uvVertices: UVVertices, var normals: Normals, var faces: Faces) {
    // Compile model to display list for faster drawing
    lazy val displayList = {
      val displayList = glGenLists(1)
      glNewList(displayList, GL_COMPILE)
      var n = -1
      for(f <- faces.sortWith(_.length < _.length)) {
        if(f.length != n || f.length >= 5) {
          if(n != -1) glEnd
          n = f.length
          n match {
            case 1 => glBegin(GL_POINTS)
            case 2 => glBegin(GL_LINES)
            case 3 => glBegin(GL_TRIANGLES)
            case 4 => glBegin(GL_QUADS)
            case _ => glBegin(GL_POLYGON)
          }
        }

        for((vi, vti, vni) <- f) {
          if(vni != -1) glNormal3d(normals(vni).x, normals(vni).y, normals(vni).z)
          if(vti != -1) glTexCoord2d(uvVertices(vti).u, uvVertices(vti).v)
          glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
        }
      }
      if(n != -1) glEnd
      
      //wireframe experiment
      /*if(lines) for(f <- faces) {
        glColor4d(0,0,0,1)
        glBegin(GL_LINE_STRIP)
        for((vi, vti, vni) <- f) {
          glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
        }
        glEnd()
      }*/
      glEndList()

      vertices = Vector.empty
      uvVertices = Vector.empty
      normals = Vector.empty
      faces = Vector.empty

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

    def render(transform: TransformLike = transform, tex: Int = tex, color: Color = color, alpha: Double = alpha) {
      render3D {
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

  class Trail(var points: Vector[Vec] = Vector.empty) {
    def +=(t: Vec) { points :+= t }

    def render() {
      render3D {
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

  case class Pixel(var x: Double, var y: Double, var transformVector: Vec = Vec0, color: Color, var dead: Boolean = false, var g: Double = 1, var acc: Double = 1.5) {
    def render() {
      transformVector = transformVector * 0.8 + Vec.random11/20
      x += transformVector.x
      y += transformVector.y + g
      g += acc
      if(y > 1080) dead = true

      glColor4d(color.r/(g/10), color.g/(g/10), color.b/(g/10), 1)
      glVertex3d(x, y, 0)
      glVertex3d(x+g, y, 0)
      glVertex3d(x+g, y+g, 0)
      glVertex3d(x,   y+g, 0)
      /*def tryset(x: Int, y: Int): Boolean = {
        if(!Liminoid.backpixelBuffer(x)(y)) {
          Liminoid.backpixelBuffer(x)(y) = true
          glColor4d(color.r/(g/10), color.g/(g/10), color.b/(g/10), 1)
          glVertex3d(x, y, 0)
          glVertex3d(x+g, y, 0)
          glVertex3d(x+g, y+g, 0)
          glVertex3d(x,   y+g, 0)
          true
        } else {
          false
        }
      }

      if(y < 1080-1 && y >= 0+1 && x >= 0+1 && x < 1920-1) {
        (tryset(x.toInt, y.toInt) 
          || tryset(x.toInt+1, y.toInt) || tryset(x.toInt-1, y.toInt) 
          || tryset(x.toInt, y.toInt+1) || tryset(x.toInt, y.toInt-1)
          || tryset(x.toInt+1, y.toInt+1) || tryset(x.toInt-1, y.toInt-1) 
          || tryset(x.toInt-1, y.toInt+1) || tryset(x.toInt+1, y.toInt-1)
        )
      }*/  
    }
  }

  // See dis? I need dis rendered
  def render3D(r: => Unit) {
    renderMode match {
      case Normal() => 
        r
      case Stereo() =>
        val x = 0.5f//Liminoid.testNum
        val x2 = 126+30+22//+Liminoid.testNum
        glEnable(GL_SCISSOR_TEST)
        glPushMatrix
          glScissor(0,0,winWidth/2, winHeight);
          cam.look(Vec3(-x,0,0), Vec3(-x2,0,0))
          r
        glPopMatrix
        glPushMatrix
          glScissor(winWidth/2,0,winWidth/2, winHeight);
          cam.look(Vec3(x,0,0), Vec3(x2,0,0))
          r
        glPopMatrix
        glDisable(GL_SCISSOR_TEST)
    }
  }
  
  val cam2D = new Camera
  cam2D.setViewPort(0,0,winWidth,winHeight)
  cam2D.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  
  case class Coord(x: Double, y: Double, w: Double, h: Double) {
    def +(d: Double): Coord = Coord(x - d/2, y - d/2, w + d, h + d)
  }

  def quad(coord: Coord, texture: Int = -1, flipx: Boolean = false, flipy: Boolean = false, alpha: Double = 1) {
    render2D {
      glDisable(GL_DEPTH_TEST)
      glDisable(GL_LIGHTING)
      
      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      
      glEnable(GL_TEXTURE_2D)
      glBindTexture(GL_TEXTURE_2D, texture)

      glPushMatrix()
        glTranslated(coord.x,coord.y,0)
        glColor4d(1,1,1,alpha)
        val (v0,v1) = if(flipy) (0f,1f) else (1f,0f)
        val (h0,h1) = if(flipx) (0f,1f) else (1f,0f)
        glBegin(GL_QUADS)
          glTexCoord2f(h1,v0); glVertex3d(0,       coord.h, 0)
          glTexCoord2f(h0,v0); glVertex3d(coord.w, coord.h, 0)
          glTexCoord2f(h0,v1); glVertex3d(coord.w, 0,       0)
          glTexCoord2f(h1,v1); glVertex3d(0,       0,       0)
        glEnd()
      glPopMatrix()

      glDisable(GL_TEXTURE_2D)

      glDisable(GL_BLEND)

      glEnable(GL_LIGHTING)
      glEnable(GL_DEPTH_TEST)
    }
  }

  def render2D(r: => Unit) {
    renderMode match {
      case Normal() => 
        cam2D.render
        r
      case Stereo() =>
        cam2D.render
        glEnable(GL_SCISSOR_TEST)
        glPushMatrix
          glScissor(0,0,winWidth/2, winHeight);
          glTranslated(-winWidth/4-eyeCorrection,0,0)
          r
        glPopMatrix
        glPushMatrix
          glScissor(winWidth/2,0,winWidth/2, winHeight);
          glTranslated(winWidth/4+eyeCorrection,0,0)
          r
        glPopMatrix
        glDisable(GL_SCISSOR_TEST)
    }
  }

  /*def displayList(r: => Unit): Int = {
    val displayList = glGenLists(1)
    glNewList(displayList, GL_COMPILE)
    r
    glEndList()

    displayList
  }*/
  
}
