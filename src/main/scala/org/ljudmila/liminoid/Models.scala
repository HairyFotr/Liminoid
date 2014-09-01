package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{ winWidth, winHeight, renderMode, eyeCorrection, testNum1, osc1 }
import scala.collection.mutable
import java.io.File
import scala.util.Random._
import scala.language.implicitConversions
import scala.annotation.switch
import Render.{ render3D, render2D }
import Utils.{ TableRandom, pow2, getRatio, SettingsReader }
import GLadDOnS._

final object Models {
  private[this] val modelCache = mutable.AnyRefMap[String, DisplayModel]()

  type Vertex = Vec
  type Face = Array[(Int, Int, Int)] //Array of indices vertex,tex,normal
  type Normal = Vec
  type UVVertex = UV

  type Vertices = Vector[Vertex]
  type Faces = Vector[Face]
  type Normals = Vector[Normal]
  type UVVertices = Vector[UVVertex]

  final object OBJModels {
    def apply(
        str: String,
        baseTransform: Transform = transform001,
        baseTransformVector: Transform = transform000): Array[Model] = {
      
      str.split("\n-+\n")
        .filterNot { str => str.trim().isEmpty() }
        .map{ str => 
          OBJModel.apply(
              str,
              baseTransform,
              baseTransformVector)
        }
      
    }
  }
  final object OBJModel {
    def apply(
        str: String,
        baseTransform: Transform = transform001,
        baseTransformVector: Transform = transform000): Model = {
      
      val default = ""
      val reader = SettingsReader(str) withDefaultValue default
      load(reader("model"))
        .toModel(
            baseTransform + Transform(vec(reader("pos")), vec(reader("rot")), vec(reader("size"))),
            baseTransformVector,
            color = color(reader("color")),
            coreTransform = Transform(vec(reader("transform")), vec0, vec0))
            //alpha, phi, theta, baseVector, coreTransform)
    }
    
    def load(filename: String): DisplayModel = modelCache.getOrElseUpdate(filename, {
      val file = io.Source.fromFile(filename)

      val vertices   = Vector.newBuilder[Vertex]
      val faces      = Vector.newBuilder[Face]
      val normals    = Vector.newBuilder[Normal]
      // Don't currently need UV, uncomment //uv/ if needed
      //uv/val uvVertices = Vector.newBuilder[UVVertex]

      for(line <- file.getLines.buffered) {
        val x = line.split(" ")
        
        (line.charAt(0): @switch) match {
          case 'v' => (line.charAt(1): @switch) match {
            case ' ' => vertices  += Vec(x(1).toDouble, x(2).toDouble, x(3).toDouble)
            case 'n' => normals   += Vec(x(1).toDouble, x(2).toDouble, x(3).toDouble)
            case 't' => //uv/uvVertices += UV(x(1).toDouble, x(2).toDouble)
          }
          
          case 'f' =>
            faces += (x.tail.map { face =>
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

      (new RawModel(vertices.result(), /*//uv/uvVertices.result(),*/ normals.result(), faces.result())).toDisplayModel
    })

    def preload(files: Array[File], max: Int = -1): Unit = {
      (if(max == -1) files else files.take(max))
        .filterNot { file => modelCache.contains(file.toString) }
        .par.foreach { file =>
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
    def withX(d: Double): Vec = Vec(d, y, z)
    def withY(d: Double): Vec = Vec(x, d, z)
    def withZ(d: Double): Vec = Vec(x, y, d)
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
  final def vec(s: String): Vec = {
    if(s.isEmpty()) vec0
    else {
      val split = s.split(" *, *")
      if(split.size == 1) vec(s.toDouble)
      else Vec(split(0).toDouble, split(1).toDouble, split(2).toDouble)
    }
  } 
  final def vec(d: Double) = Vec(d, d, d)
  final def vecx(d: Double) = Vec(d, 0, 0)
  final def vecy(d: Double) = Vec(0, d, 0)
  final def vecz(d: Double) = Vec(0, 0, d)
  final val vec0 = vec(0)
  final val vec05 = vec(0.5)
  final val vec1 = vec(1)
  final val vec2 = vec(2)
  final val vec3 = vec(3)
  final val vec4 = vec(4)
  final val vec5 = vec(5)
  final val vec90x = vecx(90)

  sealed trait TransformLike {
    def pos: Vec
    def rot: Vec
    def size: Vec

    def **(d: Double): Transform = Transform(this.pos * d, this.rot * d, this.size * d)
    def +(t: Transform): Transform = Transform(this.pos + t.pos, this.rot + t.rot, this.size + t.size)
  }
  case class Transform(val pos: Vec = vec0, val rot: Vec = vec0, val size: Vec = vec0) extends TransformLike
  implicit def mutableTransform(it: Transform): MutableTransform = MutableTransform(it.pos, it.rot, it.size) //meh
  implicit def imutableTransform(mt: MutableTransform): Transform = Transform(mt.pos, mt.rot, mt.size) //meh
  case class MutableTransform(var pos: Vec = vec0, var rot: Vec = vec0, var size: Vec = vec0) extends TransformLike {
    def setPosX(d: Double): Unit = { pos = pos.withX(d) }
    def setPosY(d: Double): Unit = { pos = pos.withY(d) }
    def setPosZ(d: Double): Unit = { pos = pos.withZ(d) }

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
  final def color(s: String): Color = {
    if(s.isEmpty()) grey0
    else {
      val split = s.split(" *, *")
      if(split.size == 1) grey(s.toDouble)
      else Color(split(0).toDouble, split(1).toDouble, split(2).toDouble)
    }
  }
  final def grey(d: Double): Color = Color(d, d, d)
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
  
  class RawModel(vertices: Vertices, /*//uv/uvVertices: UVVertices,*/ normals: Normals, faces: Faces) {
    // Compile model to display list for faster drawing
    def toDisplayModel = 
      new DisplayModel(glDisplayList {
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
            //uv/if(vti != -1) glTexCoord2d(uvVertices(vti).u, uvVertices(vti).v)
            glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
          }
        }
        if(n != -1) glEnd
        
        //wireframe experiment
        /*if(lines) for(f <- faces) {
          glColor3d(0, 0, 0)
          glPrimitive(GL_LINE_STRIP) {
            for((vi, vti, vni) <- f) {
              glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
            }
          }
        }*/
      })
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
        glCapability(GL_DEPTH_TEST, GL_LIGHTING, GL_BLEND) {
        glBlendFuncTheUsual
        if(tex != -1) {
            glEnable(GL_TEXTURE_2D)
            glBindTexture(GL_TEXTURE_2D, tex)
          } else {
            glDisable(GL_TEXTURE_2D)
          }
  
          glMatrix {
            glTranslated(pos.x, pos.y, pos.z)
            glRotated(rot.x, 1, 0, 0)
            glRotated(rot.y, 0, 1, 0)
            glRotated(rot.z, 0, 0, 1)
            glScaled(size.x, size.y, size.z)
            glColor4d(color.r, color.g, color.b, alpha)
            glCallList(displayList)
          }
  
          glDisable(GL_TEXTURE_2D)
        }
      }
    }
  }

  case class RenderProcessData(
      beat: Boolean = false)

  case class RenderRenderData(
      camx: Double, camy: Double,
      camw: Double, camh: Double)
      
  trait RenderObject {
    def init(): Unit
    def process(implicit data: RenderProcessData): Unit
    def render(implicit data: RenderRenderData): Unit
  }

  object ThreadNetwork {
    def apply(str: String): ThreadNetwork = {
      var initNodes = Vector.empty[Point]
      var nodes = Vector.empty[Point]
      var lineStr = Vector.empty[(String, String)]
      var lines = Vector.empty[Line]
		  var liminoidTexMap = Map.empty[Point, Int] withDefaultValue -1
      var phase = 0
      
      str.split("\n").map{ line =>
        line.replaceAll("[ \"]", "")
      }.filterNot{ line => 
        line.isEmpty() || line.startsWith("#") || line.startsWith("//")
      }.map{ line =>
        if (line == "-") phase += 1
        else phase match {
          case 0 =>
            initNodes :+= Point(line.split(","))
          case 1 =>
            val strings = line.split(",")  
            val node = Point(strings)
            nodes :+= node 
            if(strings.size > 2) liminoidTexMap += node -> Texture(strings(2))
          case 2 =>
            val strings = line.split("->")
            def parse(str: String): Point = 
              if(str(0) != 'i') nodes(str.toInt)
              else initNodes(str.tail.toInt)

            lines :+= Line(parse(strings(0)), parse(strings(1)))
        }
      }

      val threadMap = lines.map{ line => line -> Thread.generateMultiThread(3)(line) }.toMap

      val threadNodes = 
          nodes.map{ node => 
            ThreadNode(
              node,
              ins  = lines.filter{ _.p2 eq node }.flatMap(threadMap),
              outs = lines.filter{ _.p1 eq node }.flatMap(threadMap),
              liminoidTexMap(node))
          }
        val initThreadNodes = 
          initNodes.map{ node => 
            ThreadNode(
              node,
              ins  = Vector.empty,
              outs = lines.filter{ _.p1 eq node }.flatMap(threadMap),
              liminoidTexMap(node))
          }
        
        return ThreadNetwork(initThreadNodes, threadNodes, lines);
    }
  }

  case class ThreadNetwork(initNodes: Vector[ThreadNode], nodes: Vector[ThreadNode], lines: Vector[Line]) extends RenderObject {
    def init(): Unit = {
      nodes.foreach { node => 
        node.init()
      }
      //val noparentThreads = nodes.flatMap(_.ins).toSet &~ nodes.flatMap(_.outs).toSet;
      initNodes.foreach { node => 
        node.init()
        node.outs.foreach(_.init())
      }
    }
    def process(implicit data: RenderProcessData): Unit = {
      initNodes.foreach(_.process);
      nodes.foreach(_.process);
    }
    def render(implicit data: RenderRenderData): Unit = {
      initNodes.foreach(_.render);
      nodes.foreach(_.render);
    }
  }

  case class ThreadNode(
      position: Point,
      ins: Vector[Thread],
      outs: Vector[Thread],
      texture: Int) extends RenderObject {
    
    var outsInitialized = false
    
    def visible = 
      if(ins.isEmpty) 0 else ins.map{ thread => min(thread.visible/3, 1d) }.sum
        
    def fullyVisible = 
      ins.isEmpty || ins.maxBy{ thread => thread.visible }.visible >= 1d
    
    def init(): Unit = {
      //
    }
    
    def process(implicit data: RenderProcessData) {
      if (fullyVisible) {
        if (!outsInitialized) {
      	  println("Outs initialized")
          outsInitialized = true
          for(out <- outs) out.init()
        }
        for(out <- outs) out.process
      } 
      //for(in <- ins) if(in.isInitialized) in.process
    }

    def render(implicit data: RenderRenderData) {
      val liminoidSize = 100
      val coords = Coord(position.x-liminoidSize/2, position.y-liminoidSize/2, liminoidSize, liminoidSize)
      quad(coords, texture, false, false, visible,
          preRender = {
            glPushMatrix
            glTranslated(data.camx, data.camy, 0)
            glScaled(data.camw/1920d, data.camh/1080d, 1)
          },
          postRender = {
            glPopMatrix
          })
      glCapability(GL_BLEND) {
        glBlendFuncTheUsual
        render2D {
          glPushMatrix
          glTranslated(data.camx, data.camy, 0)
          glScaled(data.camw/1920d, data.camh/1080d, 1)
          for(in <- ins) in.render
          glPopMatrix
        }
      }
    }
  }

  final object Point {
    def apply(str: Array[String]): Point = Point(str(0).toDouble, str(1).toDouble)
  }  
  case class Point(x: Double, y: Double) {
    def toTuple() = (x, y)
  }
  object ThreadPoint {
    val visibilityThreshold = 0.0001d
    val visibilityVelocity = 0.002d
  }
  case class ThreadPoint(var desiredx: Double, var desiredy: Double) {
    var x: Double = desiredx
    var y: Double = desiredy
    var i: Double = 0.5d
    var xv: Double = 0d
    var yv: Double = 0d
    var iv: Double = 0d
    var ivv: Double = 0d
    var visible: Double = 0d
    var visiblev: Double = 0d
    var ratio = 0.1
    var paused = false
    
    def isVisible(): Boolean = visible > ThreadPoint.visibilityThreshold
    def fullyVisible(): Boolean = visible >= 1d
    
    def pause() = { paused = true }
    def unPause() = { paused = false }
    
    def init(): Unit = {
      visiblev = ThreadPoint.visibilityVelocity
      visible = ThreadPoint.visibilityThreshold + visiblev
    }
    
    var children: Vector[ThreadPoint] = Vector.empty
        
    def dist(that: ThreadPoint): Double = sqrt(pow2(this.x-that.x) + pow2(this.y-that.y))
  }
  
  final object Line {
    def apply(a: Array[String]): Line = 
      Line(
        Point(a(0).toDouble, a(1).toDouble), 
        Point(a(2).toDouble, a(3).toDouble))
  }
  case class Line(p1: Point, p2: Point) {
    def line = Vector(p1, p2)
  }

  object Thread {
    def generateMultiThread(n: Int)(l: Line): Vector[Thread] = {
      Vector.fill(n)(generateThread(l))
    }
    def generateThread(l: Line): Thread = {
      val thread = 
        Thread(l.line.sliding(2).flatMap { case s =>
          val (sx, sy) = s(0).toTuple
          val (dx, dy) = s(1).toTuple
          val segments = max(5, math.hypot(sx-dx, sy-dy)/20d);
          var prev: Option[ThreadPoint] = None
          Vector.tabulate(segments.toInt){ i => 
            val (ratio1, ratio2) = getRatio(1 - i/segments)
            val out = 
              if(i == 0)
                ThreadPoint(sx, sy)
              else if(i == segments.toInt-1)
                ThreadPoint(dx, dy)
              else
                ThreadPoint(sx*ratio1+dx*ratio2 + TableRandom.nextGaussian/7d, sy*ratio1+dy*ratio2 + TableRandom.nextGaussian/7d)

            //if(i == segments.toInt-1) out.pause
            prev.foreach { _.children = Vector(out) }
            prev = Some(out)
            out
          }
        }.toVector)
        
      thread.init()
      
      thread
    }
  }
  case class Thread(var nodes: Vector[ThreadPoint]) {
    var currentLength = 1
    
    def nodesLast = nodes.last//(nodes.size-2)
    def visible(): Double = nodesLast.visible
    def fullyVisible(): Boolean = nodesLast.fullyVisible
    
    var isInitialized = false
    def init(): Unit = {
      isInitialized = true
      nodes.head.init()
    }
    
    def process(implicit data: RenderProcessData): Unit = {
      var i = 0
      do {
        val node = nodes(i)
        //node.x += node.xv
        //node.y += node.yv
        node.x = node.desiredx*(node.ratio) + node.x*(1 - node.ratio) + TableRandom.nextGaussian/4d
        node.y = node.desiredy*(node.ratio) + node.y*(1 - node.ratio) + TableRandom.nextGaussian/4d
        //node.desiredx = node.desiredx*0.96 + node.x*0.04
        //node.desiredy = node.desiredy*0.96 + node.y*0.04
        if(i > 0) {
          val prev = nodes(i-1)
          val dx = (node.desiredx-node.x)
          val dy = (node.desiredy-node.y)
          prev.x = prev.x + dx/10d
          prev.y = prev.y + dy/10d
        }
        node.i += node.iv
        node.iv += node.ivv
        node.visible += node.visiblev
        if(node.i > 0.9) node.ivv = -0.01
        else if(node.i < 0.75) node.ivv = +0.01
        
        if(node.i < 0.6) node.i = 0.6
        
        /*if(i < nodes.length) {
          if(nodes(i-1) dist node > 5) {
            //TODO: point it back or add nodes
          }
        }*/
        
        i += 1
      } while(i < currentLength && !nodes(i).paused)
      if(data.beat && i < nodes.length && (i >= currentLength || nodes(i).paused)) { 
        if(nodes(i).paused) {
          if(nodes(i-1).visible > 0.9) {
            nodes(i).unPause
          }
        } else {
          // Init next node
          currentLength += 1
          if(currentLength > nodes.length) currentLength = nodes.length
          nodes(i).init()
        }
      }
    }
    
    def render(implicit data: RenderRenderData): Unit = {
      glLineWidth((2d + (testNum1/10d)*(osc1+1)).toFloat)
      glPrimitive(GL_LINE_STRIP) {
        var i = 0
        while(i < nodes.length && nodes(i).isVisible) {
          val node = nodes(i)
          if(node.y < 0-20 || node.y > 1080+20) {
            //nop
          } else if(i > 0 && node.visible < 1d) {
            val prevNode = nodes(i-1)
            glColor4d(node.i, node.i, node.i, node.visible)
            //glColor3d(node.i, node.i, node.i)
            val (ratio1, ratio2) = getRatio(node.visible)
            glVertex2d(node.x*ratio1 + prevNode.x*ratio2, node.y*ratio1 + prevNode.y*ratio2)
          } else {
            glColor3d(node.i, node.i, node.i)
            glVertex2d(node.x, node.y)
          }
          
          i += 1
        }
      }
    }
  }

  case class Pixel(val sx: Double, val sy: Double, var transformVector: Vec = vec0,
    color: Color, var colorg: Double = 70,
    var isDead: Boolean = false, var g: Double = 0, var acc: Double = 0.75) {
      
    var x = sx
    var y = sy

    val ssize = TableRandom.nextGaussianUnsafe*2.5

    def render(): Unit = {
      val randVec = {
        val randVec = Vec.randomGaussian(0.007)

        if(Liminoid.frames % Liminoid.shakeBumpN > Liminoid.shakeBumpN/3d)
          Vec(randVec.x*5.0, randVec.y*3.4, randVec.z)
        else
          randVec
      }

      transformVector = transformVector * 0.995 + randVec
      //val actualTransformVector = transformVector

      x += transformVector.x
      y += transformVector.y + g
      //if(Liminoid.backPixelDrop) g += acc
      if(Liminoid.backPixelMerge || !Liminoid.backPixelMerged) {
        x = x*0.95 + sx*0.05
        y = y*0.95 + sy*0.05
        transformVector = transformVector*0.95
        colorg -= acc
      } else {
        colorg += acc
      }
      if(colorg <= 75) colorg = 75
      if(y > 2000) isDead = true //TODO: Why not 1080? Can they flow back?

      val size: Double = (ssize + TableRandom.nextGaussianUnsafe*0.4)/2
      val colorDiv = (colorg/50)
      if(y <= 1080) {
        glColor3d(color.r/colorDiv, color.g/colorDiv, color.b/colorDiv)
        glVertex2d(x,      y)
        glVertex2d(x+size, y)
        glVertex2d(x+size, y+size)
        glVertex2d(x,      y+size)
      }
    }
  }

  case class Coord(x: Double, y: Double, w: Double, h: Double) {
    def +(d: Double): Coord = Coord(x - d/2, y - d/2, w + d, h + d)
  }

  def quad(coord: Coord, texture: Int = -1, flipx: Boolean = false, flipy: Boolean = false, alpha: Double = 1d, color: Color = grey1, blend: (Int, Int) = (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA),
      preRender: => Unit = {},
      postRender: => Unit = {}): Unit = {
    
    glDisable(GL_DEPTH_TEST)
    glDisable(GL_LIGHTING)
    
    
    if(blend != (-1, -1)) {
      glEnable(GL_BLEND)
      glBlendFunc(blend._1, blend._2)
    } else {
      glEnable(GL_ALPHA_TEST)
      glAlphaFunc(GL_GEQUAL, 0.9f)
    }
    
    if(texture != -1) {
      glEnable(GL_TEXTURE_2D)
      glBindTexture(GL_TEXTURE_2D, texture)
    }

    
    glColor4d(color.r, color.g, color.b, alpha)

    val (v0, v1) = if(flipy) (0f, 1f) else (1f, 0f)
    val (h0, h1) = if(flipx) (0f, 1f) else (1f, 0f)
    
    render2D {
    	preRender
      glPrimitive(GL_QUADS) {
        glTexCoord2f(h1, v0); glVertex2d(coord.x,         coord.y+coord.h)
        glTexCoord2f(h0, v0); glVertex2d(coord.x+coord.w, coord.y+coord.h)
        glTexCoord2f(h0, v1); glVertex2d(coord.x+coord.w, coord.y)
        glTexCoord2f(h1, v1); glVertex2d(coord.x,         coord.y)
      }
      postRender
    }

    if(texture != -1) glDisable(GL_TEXTURE_2D)

    if(blend != (-1, -1)) {
      glDisable(GL_BLEND)
    } else {
      glDisable(GL_ALPHA_TEST)
    }

    glEnable(GL_LIGHTING)
    glEnable(GL_DEPTH_TEST)
  }  
}
