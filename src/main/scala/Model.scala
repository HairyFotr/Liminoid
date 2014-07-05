package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{ winWidth, winHeight, renderMode, RenderMode, Normal, Stereo, eyeCorrection }
import scala.collection.mutable
import java.io.File
import util.Random._
import scala.language.implicitConversions
import scala.annotation.switch
import Utils.TableRandom

final object Model {
  private[this] val modelCache = mutable.AnyRefMap[String, DisplayModel]()

  type Vertices = Vector[Vec]
  type Faces = Vector[Array[(Int, Int, Int)]] //Array of indices vertex,tex,normal
  type UVVertices = Vector[UV]
  type Normals = Vector[Vec]

  final object OBJModel {
    def apply(fileStr: String): DisplayModel = modelCache.getOrElseUpdate(fileStr, {
      val file = io.Source.fromFile(fileStr)

      var vertices: Vertices     = Vector.empty
      var faces: Faces           = Vector.empty
      var normals: Normals       = Vector.empty
      var uvVertices: UVVertices = Vector.empty

      for(line <- file.getLines.buffered) {
        val x = line.split(" ")
        
        (line.charAt(0): @switch) match {
          case 'v' => (line.charAt(1): @switch) match {
            case ' ' => vertices  :+= Vec(x(1).toDouble, x(2).toDouble, x(3).toDouble)
            case 'n' => normals   :+= Vec(x(1).toDouble, x(2).toDouble, x(3).toDouble)
            case 't' => uvVertices :+= UV(x(1).toDouble, x(2).toDouble)
          }
          
          case 'f' =>
            faces :+= (x.tail.map { face =>
              val fs = face.split("/")
              if(fs.length == 1)      (fs(0).toInt-1,            -1,            -1)
              else if(fs.length == 2) (fs(0).toInt-1, fs(1).toInt-1,            -1)
              else if(fs(1).isEmpty)  (fs(0).toInt-1,            -1, fs(2).toInt-1)
              else                    (fs(0).toInt-1, fs(1).toInt-1, fs(2).toInt-1)
            })

          case _ => // nop
        }
      }

      file.close

      (new RawModel(vertices, uvVertices, normals, faces)).toDisplayModel
    })

    def preload(files: Array[File], max: Int = -1): Unit = {
      (if(max == -1) files else files.take(max))
        .filterNot { file => modelCache.contains(file.toString) }
        .par.map { file =>
          apply(file.toString)
          //val rawModel = apply(file.toString)
          //modelCache(file.toString) = rawModel
          //rawModel
        }
        //.seq.foreach { rawModel =>
        //  rawModel.displayList
        //}
    }
  }

  sealed trait VecLike {
    def x: Double
    def y: Double
    def z: Double

    def +(v: VecLike): Vec = Vec(x+v.x, y+v.y, z+v.z)
    def -(v: VecLike): Vec = Vec(x-v.x, y-v.y, z-v.z)
    def unary_-(): Vec = Vec(-x, -y, -z)
    def *(f: Double): Vec = Vec(x*f, y*f, z*f)
    def /(f: Double): Vec = Vec(x/f, y/f, z/f)
    def distance(v: Vec): Double = sqrt(pow(x-v.x, 2) + pow(y-v.y, 2) + pow(z-v.z, 2))
    def minCoord(v: VecLike): Vec = Vec(min(x, v.x), min(y, v.y), min(z, v.z))
    def maxCoord(v: VecLike): Vec = Vec(max(x, v.x), max(y, v.y), max(z, v.z))
    def span(v: VecLike): Vec = Vec(abs(x-v.x), abs(y-v.y), abs(z-v.z))
    def avg(): Double = (x+y+z)/3d
    def setX(d: Double): Vec = Vec(d, y, z)
    def setY(d: Double): Vec = Vec(x, d, z)
    def setZ(d: Double): Vec = Vec(x, y, d)
    def zeroX(): Vec = Vec(0, y, z)
    def zeroY(): Vec = Vec(x, 0, z)
    def zeroZ(): Vec = Vec(x, y, 0)
    def normalize(): Vec = {
      val m = max(abs(x), max(abs(y), abs(z)))
      Vec(x/m, y/m, z/m)
    }
  }
  final object Vec {
    def random(): Vec = random01
    def random360(): Vec = random01 * 360
    def random01(): Vec = Vec(nextDouble, nextDouble, nextDouble)
    def randomGaussian(): Vec = Vec(TableRandom.nextGaussian, TableRandom.nextGaussian, TableRandom.nextGaussian)
    def randomGaussian(d: Double): Vec = Vec(TableRandom.nextGaussian*d, TableRandom.nextGaussian*d, TableRandom.nextGaussian*d)
    def randomUniform01(): Vec = { val rnd = nextDouble; Vec(rnd, rnd, rnd) }
  }
  case class Vec(val x: Double, val y: Double, val z: Double) extends VecLike
  implicit def mutableVec(it: Vec): MutableVec = MutableVec(it.x, it.y, it.z)
  case class MutableVec(var x: Double, var y: Double, var z: Double) extends VecLike {
    //def +=(v: VecLike): Unit = { x += v.x; y += v.y; z += v.z; }
    def *=(f: Double): Unit = { x *= f; y *= f; z *= f }
  }
  @inline final def vec(d: Double) = Vec(d, d, d)
  final val vec0 = vec(0)
  final val vec05 = vec(0.5)
  final val vec1 = vec(1)
  final val vec2 = vec(2)
  final val vec3 = vec(3)
  final val vec4 = vec(4)
  final val vec5 = vec(5)
  final val vec90x = Vec(90, 0, 0)

  sealed trait TransformLike {
    def pos: Vec
    def rot: Vec
    def size: Vec

    def **(d: Double): Transform = Transform(this.pos * d, this.rot * d, this.size * d)
  }
  case class Transform(val pos: Vec = vec0, val rot: Vec = vec0, val size: Vec = vec0) extends TransformLike
  implicit def mutableTransform(it: Transform): MutableTransform = MutableTransform(it.pos, it.rot, it.size) //meh
  implicit def imutableTransform(mt: MutableTransform): Transform = Transform(mt.pos, mt.rot, mt.size) //meh
  case class MutableTransform(var pos: Vec = vec0, var rot: Vec = vec0, var size: Vec = vec0) extends TransformLike {
    def setPosX(d: Double): Unit = { pos = pos.setX(d) }
    def setPosY(d: Double): Unit = { pos = pos.setY(d) }
    def setPosZ(d: Double): Unit = { pos = pos.setZ(d) }

    def +=(vector: TransformLike): Unit = {
      pos = pos + vector.pos
      rot = rot + vector.rot
      size = size + vector.size
    }
  }
  final val transform001 = Transform(vec0, vec0, vec1)
  final val transform000 = Transform(vec0, vec0, vec0)

  case class UV(u: Double, v: Double)

  case class Rotation(yaw: Float, pitch: Float, roll: Float) {
    def +(r: Rotation): Rotation = Rotation(yaw+r.yaw, pitch+r.pitch, roll+r.roll)
    def -(r: Rotation): Rotation = Rotation(yaw-r.yaw, pitch-r.pitch, roll-r.roll)
    def *(f: Float): Rotation = Rotation(yaw*f, pitch*f, roll*f)
  }
  final val rotation0 = Rotation(0, 0, 0)
  
  case class Color(var r: Double, var g: Double, var b: Double) {
    def -=(f: Double): Unit = { r -= f; g -= f; b -= f }
    def *=(f: Double): Unit = { r *= f; g *= f; b *= f }
  }
  object Color {
    def RGB(i: Int): Color = Color(((i & 255)/255d), (((i >> 8) & 255)/255d), (((i >> 16) & 255)/255d))
    def BGR(i: Int): Color = Color((((i >> 16) & 255)/256d), (((i >> 8) & 255)/256d), ((i & 255)/256d))
  }
  @inline final def grey(d: Double): Color = Color(d, d, d)
  final val grey0 = grey(0)
  final val grey1 = grey(1)

  class DisplayModel(val displayList: Int) extends AnyVal {
    def toModel( //TODO: Ouch, this sucks, but gets around mutability of the memoization cache
        transform: MutableTransform = transform001,
        transformVector: MutableTransform = transform000,
        tex: Int = -1,
        color: Color,
        alpha: Double = 1d,
        phi: Double = 2*Pi*nextDouble,
        theta: Double = 0,
        baseVector: Vec = vec0,
        coreTransform: MutableTransform = transform000): Model = Model(displayList, transform, transformVector, tex, color, alpha, phi, theta, baseVector, coreTransform)
  }
  
  class RawModel(vertices: Vertices, uvVertices: UVVertices, normals: Normals, faces: Faces) {
    // Compile model to display list for faster drawing
    def toDisplayModel = {
      val displayList = glGenLists(1)
      glNewList(displayList, GL_COMPILE)
      var n = -1
      for(f <- faces.sortWith(_.length < _.length)) {
        if(f.length != n || f.length >= 5) {
          if(n != -1) glEnd
          n = f.length
          glBegin((n: @switch) match {
            case 1 => GL_POINTS
            case 2 => GL_LINES
            case 3 => GL_TRIANGLES
            case 4 => GL_QUADS
            case _ => GL_POLYGON
          })
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
        glColor4d(0, 0, 0, 1)
        glBegin(GL_LINE_STRIP)
        for((vi, vti, vni) <- f) {
          glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
        }
        glEnd()
      }*/
      glEndList()

      /*vertices = Vector.empty
      uvVertices = Vector.empty
      normals = Vector.empty
      faces = Vector.empty*/

      new DisplayModel(displayList)
    }
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
      var baseVector: Vec,
      var coreTransform: MutableTransform) {

    def render(transform: TransformLike = transform, tex: Int = tex, color: Color = color, alpha: Double = alpha): Unit = {
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
          glRotated(rot.x, 1, 0, 0)
          glRotated(rot.y, 0, 1, 0)
          glRotated(rot.z, 0, 0, 1)
          glScaled(size.x, size.y, size.z)
          glColor4d(color.r, color.g, color.b, alpha)
          glCallList(displayList)
        glPopMatrix()

        glDisable(GL_TEXTURE_2D)

        glDisable(GL_BLEND)

        glEnable(GL_LIGHTING)
        glEnable(GL_DEPTH_TEST)
      }
    }
  }

  /*class Trail(var points: Vector[Vec] = Vector.empty) {
    def +=(t: Vec): Unit = { points :+= t }

    def render(): Unit = {
      glEnable(GL_BLEND)
      glEnable(GL_DEPTH_TEST)
      glEnable(GL_LIGHTING)
      render3D {
        val take = 100
        val error = Vec.randomGaussian(0.2)
        points = points.takeRight(take)
        glBegin(GL_LINE_STRIP)
        var i = take.toDouble
        for(v <- points.reverse) {
          val vec = v //+ (error * (take-i))
          glColor4d(0, 0, 0, i/take)
          glVertex3d(vec.x, vec.y, vec.z)
          //glVertex3d(vec.x+1, vec.y+1, vec.z+1)
          i -= 1
        }
        glEnd()
      }
      glDisable(GL_BLEND)
      glDisable(GL_DEPTH_TEST)
      glDisable(GL_LIGHTING)
    }
  }*/

  case class Pixel(var x: Double, var y: Double, var transformVector: Vec = vec0,
    color: Color, var colorg: Double = 70,
    var isDead: Boolean = false, var g: Double = 0, var acc: Double = 0.75) {

    val ssize = TableRandom.nextGaussian2*2.5

    def render(): Unit = {
      val randVec = {
        val randVec = Vec.randomGaussian(0.007)

        if(Liminoid.frames % Liminoid.shakeBumpN > Liminoid.shakeBumpN/3d)
          Vec(randVec.x*5.0, randVec.y*3.4, randVec.z)
        else
          randVec
      }
      

      transformVector = transformVector * 0.995 + randVec
      val actualTransformVector = transformVector

      x += actualTransformVector.x
      y += actualTransformVector.y + g
      if(Liminoid.backPixelDrop) {
        g += acc
      }
      colorg += acc
      if(y > 2080) isDead = true //TODO: Why not 1080? Can they flow back?

      val size: Double = (ssize + TableRandom.nextGaussian2*0.4)/2
      val colorDiv = (colorg/50)
      if(y <= 1080) {
        glColor3d(color.r/colorDiv, color.g/colorDiv, color.b/colorDiv)
        glVertex2d(x,      y)
        glVertex2d(x+size, y)
        glVertex2d(x+size, y+size)
        glVertex2d(x,      y+size)
      }
      /*def tryset(x: Int, y: Int): Boolean = {
        if(!Liminoid.backpixelBuffer(x)(y)) {
          Liminoid.backpixelBuffer(x)(y) = true
          glColor4d(color.r/(g/10), color.g/(g/10), color.b/(g/10), 1)
          glVertex2d(x,   y)
          glVertex2d(x+g, y)
          glVertex2d(x+g, y+g)
          glVertex2d(x,   y+g)
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

  lazy val cam = {
    val cam = new Camera
    cam.setViewPort(0,0, winWidth,winHeight)
    cam.setOrtho(0,winHeight,winWidth,0, 1f,-1f)
    cam.setPerspective(50, winWidth/winHeight.toFloat, 0.25f, 700f)
    cam.setPosition(0, 0, 0)
    cam.lookAt(Vec3(0, 0, 200))
    
    cam
  }

  // See dis? I need dis rendered
  def render3D(r: => Unit): Unit = {
    renderMode match {
      case Normal =>
        r
      case Stereo =>
        //TODO: name these
        val x = 0.5f       //+ Liminoid.testNum
        val x2 = 126+30+22 //+ Liminoid.testNum
        glEnable(GL_SCISSOR_TEST)
        glPushMatrix
          glScissor(0,0, winWidth/2,winHeight)
          cam.look(Vec3(-x, 0, 0), Vec3(-x2, 0, 0))
          r
        glPopMatrix
        glPushMatrix
          glScissor(winWidth/2,0, winWidth/2,winHeight)
          cam.look(Vec3(+x, 0, 0), Vec3(+x2, 0, 0))
          r
        glPopMatrix
        glDisable(GL_SCISSOR_TEST)
    }
  }
  
  lazy val cam2D = {
    val cam2D = new Camera
    cam2D.setViewPort(0,0, winWidth,winHeight)
    cam2D.setOrtho(0,winHeight,winWidth,0, 1f,-1f)
    
    cam2D
  }
  
  case class Coord(x: Double, y: Double, w: Double, h: Double) {
    def +(d: Double): Coord = Coord(x - d/2, y - d/2, w + d, h + d)
  }

  def quad(coord: Coord, texture: Int = -1, flipx: Boolean = false, flipy: Boolean = false, alpha: Double = 1d, color: Color = grey1, blend: (Int, Int) = (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)): Unit = {
    glDisable(GL_DEPTH_TEST)
    glDisable(GL_LIGHTING)
    
    glEnable(GL_BLEND)
    glBlendFunc(blend._1, blend._2)
    
    if(texture != -1) {
      glEnable(GL_TEXTURE_2D)
      glBindTexture(GL_TEXTURE_2D, texture)
    }
    glColor4d(color.r, color.g, color.b, alpha)

    val (v0, v1) = if(flipy) (0f, 1f) else (1f, 0f)
    val (h0, h1) = if(flipx) (0f, 1f) else (1f, 0f)
    
    render2D {
      glBegin(GL_QUADS)
        glTexCoord2f(h1, v0); glVertex2d(coord.x,         coord.y+coord.h)
        glTexCoord2f(h0, v0); glVertex2d(coord.x+coord.w, coord.y+coord.h)
        glTexCoord2f(h0, v1); glVertex2d(coord.x+coord.w, coord.y)
        glTexCoord2f(h1, v1); glVertex2d(coord.x,         coord.y)
      glEnd()
    }
    if(texture != -1) glDisable(GL_TEXTURE_2D)

    glDisable(GL_BLEND)

    glEnable(GL_LIGHTING)
    glEnable(GL_DEPTH_TEST)
  }

  def render2D(toRender: => Unit): Unit = {
    renderMode match {
      case Normal =>
        cam2D.render
        toRender
      case Stereo =>
        cam2D.render
        glEnable(GL_SCISSOR_TEST)
          val eyeOffset = (winWidth/4+eyeCorrection)
          glScissor(0,0, winWidth/2,winHeight)
          glTranslated(-eyeOffset, 0, 0)
          toRender

          glScissor(winWidth/2,0, winWidth/2,winHeight)
          glTranslated(2*eyeOffset, 0, 0)
          toRender
          glTranslated(-eyeOffset, 0, 0)
        glDisable(GL_SCISSOR_TEST)
    }
  }

  /*def displayList(toRender: => Unit): Int = {
    val displayList = glGenLists(1)
    glNewList(displayList, GL_COMPILE)
    toRender
    glEndList()

    displayList
  }*/
  
}
