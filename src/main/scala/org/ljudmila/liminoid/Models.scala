package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{ winWidth, winHeight, renderMode, eyeCorrection, testNum1, testNum2, testNum3, osc1 }
import scala.collection.mutable
import java.io.File
import scala.util.Random._
import scala.language.implicitConversions
import scala.annotation.switch
import Render.{ render3D, render2D }
import org.ljudmila.SettingsReader
import org.ljudmila.Utils.{ TableRandom, pow2, getRatio, withAlternative }
import GLadDOnS._
import org.ljudmila.hardware.Sound

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

  object OBJModels {
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
  object OBJModel {
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
            spread = (withAlternative(reader("spreadx").toDouble, 0), withAlternative(reader("spready").toDouble, 0)),
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
  object Vec {
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
  def vec(s: String): Vec = {
    if(s.isEmpty()) vec0
    else {
      val split = s.split(" *, *")
      if(split.size == 1) vec(s.toDouble)
      else Vec(split(0).toDouble, split(1).toDouble, split(2).toDouble)
    }
  }
  def vec(d: Double) = Vec(d, d, d)
  def vecx(d: Double) = Vec(d, 0, 0)
  def vecy(d: Double) = Vec(0, d, 0)
  def vecz(d: Double) = Vec(0, 0, d)
  val vec0 = vec(0)
  val vec05 = vec(0.5)
  val vec1 = vec(1)
  val vec2 = vec(2)
  val vec3 = vec(3)
  val vec4 = vec(4)
  val vec5 = vec(5)
  val vec90x = vecx(90)

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
    def setPos(d: Double): Unit = { pos = pos.withZ(d) }

    def +=(vector: TransformLike): Unit = {
      pos = pos + vector.pos
      rot = rot + vector.rot
      size = size + vector.size
    }
  }
  val transform001 = Transform(vec0, vec0, vec1)
  val transform000 = Transform(vec0, vec0, vec0)

  case class UV(u: Double, v: Double)

  case class Rotation(yaw: Float, pitch: Float, roll: Float) {
    def +(r: Rotation): Rotation = Rotation(yaw+r.yaw, pitch+r.pitch, roll+r.roll)
    def -(r: Rotation): Rotation = Rotation(yaw-r.yaw, pitch-r.pitch, roll-r.roll)
    def *(f: Float): Rotation = Rotation(yaw*f, pitch*f, roll*f)
  }
  val rotation0 = Rotation(0, 0, 0)
  
  case class Color(var r: Double, var g: Double, var b: Double) {
    def -=(f: Double): Unit = { r -= f; g -= f; b -= f }
    def *=(f: Double): Unit = { r *= f; g *= f; b *= f }

    def *(f: Double): Color = Color(r * f, g * f, b * f)

    def toRGB(): Int = 
      ((r*255).toInt) + 
      ((g*255).toInt >> 8) +
      ((b*255).toInt >> 16)
  }
  object Color {
    def RGB(i: Int): Color = Color(((i & 255)/255d), (((i >> 8) & 255)/255d), (((i >> 16) & 255)/255d))
    def BGR(i: Int): Color = Color((((i >> 16) & 255)/256d), (((i >> 8) & 255)/256d), ((i & 255)/256d))
    def Gray(i: Int): Color = Color((i & 255)/255d, (i & 255)/255d, (i & 255)/255d)
  }
  def color(s: String): Color = {
    if(s.isEmpty()) grey0
    else {
      val split = s.split(" *, *")
      if(split.size == 1) grey(s.toDouble)
      else Color(split(0).toDouble, split(1).toDouble, split(2).toDouble)
    }
  }
  def grey(d: Double): Color = Color(d, d, d)
  val grey0 = grey(0)
  val grey1 = grey(1)

  class DisplayModel(val displayList: Int) extends AnyVal {
    def toModel( //TODO: Ouch, this sucks, but gets around mutability of the memoization cache
        transform: MutableTransform = transform001,
        transformVector: MutableTransform = transform000,
        tex: Int = -1,
        spread: (Double, Double) = (0, 0),
        color: Color,
        alpha: Double = 1d,
        phi: Double = 2*Pi*nextDouble,
        theta: Double = 0,
        baseVector: Vec = vec0,
        coreTransform: MutableTransform = transform000): Model = {
      
      Model(displayList, transform, transformVector, tex, color, alpha, phi, theta, baseVector, coreTransform, spread)
    }
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
      var color: Color,
      var alpha: Double,
      var phi: Double,
      var theta: Double,
      var baseVector: Vec,
      var coreTransform: MutableTransform, 
      var spread: (Double, Double) = (0,0)) {

    def render(transform: TransformLike = transform, tex: Int = tex, color: Color = color, alpha: Double = alpha): Unit = {
      render3D {
        glCapability(GL_DEPTH_TEST, GL_LIGHTING, GL_BLEND) {
          glTheUsualBlendFunc
          if(tex != -1) {
            glEnable(GL_TEXTURE_2D)
            glBindTexture(GL_TEXTURE_2D, tex)
          } else {
            glDisable(GL_TEXTURE_2D)
          }
          
          glMatrix {
            import transform.{ pos, rot, size }
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

  // It's made of state and fail
  case class RenderProcessData(
      beat: Boolean = false,
      pullMode: Boolean = false,
      var centerx: Double, var centery: Double,
      ratio: Double) 
  {
      val ratio1m = 1-ratio
  }

  case class RenderRenderData(
      camx: Double, camy: Double,
      camw: Double, camh: Double)
  {
    var pullingThisThread = false
  }
  trait RenderObject {
    def init(): Unit
    def process(implicit data: RenderProcessData): Unit
    def render(implicit data: RenderRenderData): Unit
  }

  object ThreadNetwork {
    // It's made of state and fail
    var threadMap: Map[Line, Vector[Thread]] = null
    var allNodes: Vector[ThreadNode] = null
    var noonePulling = true
    var setPulling = false
    
    def apply(str: String): ThreadNetwork = {
      var initNodes = Vector.empty[Point]
      var nodes = Vector.empty[Point]
      var lines = Vector.empty[Line]
		  var liminoidTexMap = Map.empty[Point, Int] withDefaultValue -1
      var danglingNodes = Vector.empty[Point]
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
            if(strings.size > 2) {
              liminoidTexMap += node -> Texture(strings(3))
            }
          case 2 =>
            danglingNodes :+= Point(line.split(","))
          case 3 =>
            try {
              var strings = line.split("->")
              var direction = 1
              if (strings.size == 1) {
                strings = line.split("<-")
                direction = -1
              }
              def parse(str: String) = {
                if(str(0) == 'i') initNodes(str.tail.toInt-1)
                else if(str(0) == 'd') danglingNodes(str.tail.toInt-1)
                else nodes(str.toInt-1)
              }
              

            lines :+= 
              (if (direction == 1) {
                Line(parse(strings(0)), parse(strings(1)))
              } else {
                Line(parse(strings(1)), parse(strings(0)))
              })
            } catch {
              case e: Exception =>
                println(initNodes)
                println(initNodes.size)
                println(nodes)
                println(nodes.size)
                throw e
            }
          case 4 => //TODO
          case _ => println("WTF mate in loading network")
        }
      }

      threadMap = lines.map{ line => line -> Thread.generateMultiThread(2)(line) }.toMap
      
      val threadNodes = 
          nodes.map{ node => 
            ThreadNode(
              node,
              insNodes  = lines.filter{ _.p2 eq node },
              outsNodes = lines.filter{ _.p1 eq node },

              liminoidTexMap(node),
              20+nextInt(15))
          }
        val initThreadNodes = 
          initNodes.map{ node => 
            ThreadNode(
              node,
              insNodes  = Vector.empty,
              outsNodes = lines.filter{ _.p1 eq node },
              liminoidTexMap(node),
              20+nextInt(15),
              InitNode)
          }
      val danglingThreadNodes = 
          danglingNodes.map{ node => 
            ThreadNode(
              node,
              insNodes  = lines.filter{ _.p2 eq node },
              outsNodes = lines.filter{ _.p1 eq node },
              -1,
              20+nextInt(15),
              DanglingNode)
          }
        
        return ThreadNetwork(initThreadNodes, threadNodes, danglingThreadNodes, lines, threadMap)
    }
  }

  case class ThreadNetwork(
      initNodes: Vector[ThreadNode],
      nodes: Vector[ThreadNode],
      danglingNodes: Vector[ThreadNode],
      lines: Vector[Line],
      threadMap: Map[Line, Vector[Thread]]) extends RenderObject {
    
    ThreadNetwork.allNodes = initNodes ++ nodes ++ danglingNodes
    
    def fullyVisible = 
      nodes.forall(_.fullyVisible) &&
      danglingNodes.forall(_.fullyVisible)
    
    def totallyVisibleNodes = 
      nodes.forall(_.totallyVisible)

    def totallyVisible = 
      nodes.forall(_.totallyVisible) &&
      danglingNodes.forall(_.totallyVisible)

    def init(): Unit = {
      for(node <- nodes) {
        node.init()
      }

      //val noparentThreads = nodes.flatMap(_.ins).toSet &~ nodes.flatMap(_.outs).toSet;
      for (node <- initNodes) { 
        node.init()
        node.outs.foreach(_.init())
      }
    }
    def process(implicit data: RenderProcessData): Unit = {
      if (data.pullMode) {
        data.centerx = danglingNodes.head.position.x
        data.centery = danglingNodes.head.position.y
        if (ThreadNetwork.setPulling) {
          ThreadNetwork.noonePulling = true
          ThreadNetwork.setPulling = false
          val nodes = ThreadNetwork.allNodes.filter { node => !node.pulling }
          if (nodes.nonEmpty) {
            nodes.minBy { node =>
              sqrt(pow2(node.position.x - data.centerx) + pow2(node.position.y - data.centery))
            }.pulling = true
          } // TODO finished pulling
        }
        danglingNodes.foreach { x => x.pulling = true }
      }
      
      initNodes.foreach(_.process)
      nodes.foreach(_.process)
      //if (totallyVisibleNodes) {
        danglingNodes.foreach(_.process)
      //}
    }
    def render(implicit data: RenderRenderData): Unit = {
      initNodes.foreach(_.renderThreads)
      nodes.foreach(_.renderThreads)
      danglingNodes.foreach(_.renderThreads)
      nodes.foreach(_.renderNode)
    }
  }

  sealed trait ThreadNodeType
  case object InitNode extends ThreadNodeType
  case object RegularNode extends ThreadNodeType
  case object DanglingNode extends ThreadNodeType
  
  case class ThreadNode(
      position: Point,
      insNodes: Vector[Line],
      outsNodes: Vector[Line],
      texture: Int,
      nodeSize: Double,
      nodeType: ThreadNodeType = RegularNode) extends RenderObject {
    
    val ins  = insNodes.flatMap(ThreadNetwork.threadMap)
    val outs = outsNodes.flatMap(ThreadNetwork.threadMap)

    var outsInitialized = false
    var pulled: Boolean = false
    var pulling: Boolean = false
    var scheduledPulling: Boolean = false
    
    def visible = 
      if(ins.isEmpty) 0 else ins.map{ thread => min(pow2(thread.backupvisible), 1d) }.max
      
    var exvisible = 0d
    def wasInvisible = {
      val out = (exvisible < 0.5) && (visible > 0.5)
      exvisible = visible
      out
    }
    
    def fullyVisible = 
      ins.isEmpty || ins.maxBy{ thread => thread.backupvisible }.backupvisible >= 1d

    def totallyVisible = 
      ins.isEmpty || ins.minBy{ thread => thread.backupvisible }.backupvisible >= 1d
    
    def init(): Unit = {
      //
    }
    
    def process(implicit data: RenderProcessData): Unit = {
      if (fullyVisible) {
        if (!outsInitialized) {
      	  println("Outs initialized")
          outsInitialized = true
          for(out <- outs) out.init()
        }
        for(out <- outs) {
          out.process
        }
      }
      if (data.pullMode) {
        if (pulling) {
          position.x = position.x*data.ratio + data.centerx*data.ratio1m
          position.y = position.y*data.ratio + data.centery*data.ratio1m
        }
        if (abs(position.x - data.centerx) + abs(position.y - data.centery) < 200 && !pulled) {
        	pulled = true
          ThreadNetwork.setPulling = true
    		  ThreadNetwork.allNodes.filter { n => 
            n.outsNodes.exists { o => (o.p2 eq position) || (o.p1 eq position) } ||
            n.insNodes.exists { o => (o.p2 eq position) || (o.p1 eq position) }
          }.foreach {
            x => x.scheduledPulling = true
          }
        }
      }
      if (wasInvisible && nodeType == RegularNode) {
        Sound.play("network"+(nextInt(6)+1));
      }
      //for(in <- ins) if(in.isInitialized) in.process
    }

    def render(implicit data: RenderRenderData): Unit = {
      renderThreads
      renderNode
    }
    def renderThreads(implicit data: RenderRenderData): Unit = {
      data.pullingThisThread = pulling
      glCapability(GL_BLEND) {
        glTheUsualBlendFunc
        render2D {
          glPushMatrix
          glTranslated(data.camx, data.camy, 0)
          glScaled(data.camw/1920d, data.camh/1080d, 1)
          for(in <- ins) in.render
          glPopMatrix
        }
      }
    }
    def renderNode(implicit data: RenderRenderData): Unit = {
      val liminoidSizex = position.s + (TableRandom.nextGaussianUnsafe/100d) * position.s
      val liminoidSizey = position.s + (TableRandom.nextGaussianUnsafe/100d) * position.s
      val posx = position.x-liminoidSizex/2 + (TableRandom.nextGaussianUnsafe/50d) * position.s
      val posy = position.y-liminoidSizey/2 + (TableRandom.nextGaussianUnsafe/50d) * position.s
      
      val coords = Coord(posx, posy, liminoidSizex, liminoidSizey)
      quad(coords, texture, false, false, if(pulling) 1 else visible,
        preRender = {
          glPushMatrix
          glTranslated(data.camx, data.camy, 0)
          glScaled(data.camw/1920d, data.camh/1080d, 1)
        },
        postRender = {
          glPopMatrix
        })
    }
  }

  object Point {
    def apply(str: Array[String]): Point = {
      if(str.size == 2)
        Point(str(0).toDouble+Liminoid.threadNetworkOffsetx, str(1).toDouble+Liminoid.threadNetworkOffsety)
      else
        Point(str(0).toDouble+Liminoid.threadNetworkOffsetx, str(1).toDouble+Liminoid.threadNetworkOffsety, str(2).toDouble) 
    }
  }  
  case class Point(var x: Double, var y: Double, var s: Double = 1.0) {
    s = s*30;
    def toTuple() = (x, y)
    //def toTuple() = (x, y, s)
  }
  object ThreadPoint {
    val visibilityThreshold = 0.00035d
    val visibilityVelocity = 0.0055d
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
  
  object Line {
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
        Thread({
          val (sx, sy) = l.p1.toTuple
          val (dx, dy) = l.p2.toTuple
          val segments = max(5, math.hypot(sx-dx, sy-dy)/30d)
          val segmentsInt = segments.toInt
          var prev: Option[ThreadPoint] = None
          Vector.tabulate(segmentsInt){ i => 
            val (ratio1, ratio2) = getRatio(1 - i/segments)
            val out = 
              if(i <= 0)
                ThreadPoint(sx, sy)
              else if(i >= segmentsInt-1)
                ThreadPoint(dx, dy)
              else
                ThreadPoint(
                    sx*ratio1+dx*ratio2 + TableRandom.nextGaussian/7d,
                    sy*ratio1+dy*ratio2 + TableRandom.nextGaussian/7d)

            prev.foreach { _.children = Vector(out) }
            
            prev = Some(out)
            out
          }
        })
      
      thread.init()
      thread
    }
  }
  case class Thread(var nodes: Vector[ThreadPoint]) {
    var currentLength = 1
    
    def nodesLast = nodes.last//(nodes.size-2)
    def backupvisible(): Double = nodesLast.visible
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
        node.x = node.desiredx*(node.ratio) + node.x*(1 - node.ratio) + TableRandom.nextGaussian/1.5d
        node.y = node.desiredy*(node.ratio) + node.y*(1 - node.ratio) + TableRandom.nextGaussian/1.5d
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
      if (data.pullMode) {
        for(node <- nodes) {
          //node.x = node.x*data.ratio + data.centerx*data.ratio1m
          //node.y = node.y*data.ratio + data.centery*data.ratio1m
          //node.desiredx = node.desiredx*data.ratio + data.centerx*data.ratio1m
          //node.desiredy = node.desiredy*data.ratio + data.centery*data.ratio1m
        }
      }
    }
    
    def render(implicit data: RenderRenderData): Unit = {
      glLineWidth((2d + (testNum2/10d)*(osc1+1)).toFloat)
      glPrimitive(GL_LINE_STRIP) {
      //glPrimitive(GL_LINES) {// Dashed
        var i = 0
        while(i < nodes.length && nodes(i).isVisible) {
        	val node = nodes(i)
          if(node.y < 0-20 || node.y > 1080+20) {
            //nop
          } else if(i > 0 && (node.visible < 1d || data.pullingThisThread)) {
            val prevNode = nodes(i-1)
            if (data.pullingThisThread) {
              node.visible = node.visible * 0.8 
              node.visiblev = 0
            }
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

  object Pixel {
    def convexHull(_points: Seq[Pixel]): Seq[Pixel] = {
      if (_points.isEmpty) return _points
      val points = _points.sortBy(_.x)
      val upper = halfHull(points)
      val lower = halfHull(points.reverse)
      upper.remove(0)
      lower.remove(0)
      upper ++: lower
    }
    
    private def halfHull(points: Seq[Pixel]) = {
      val upper = new mutable.ListBuffer[Pixel]()
      for (p <- points) {
        while (upper.size >= 2 && leftTurn(p, upper(0), upper(1))) {
          upper.remove(0)
        }
        upper.prepend(p)
      }
      upper
    }
    
    private def leftTurn(p1: Pixel, p2: Pixel, p3: Pixel) = {
      val slope = (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
      val collinear = math.abs(slope) <= 1e-9
      val leftTurn = slope < 0
      collinear || leftTurn
    }
  }
  
  case class Pixel(
      var sx: Double, var sy: Double,
      var transformVector: Vec = vec0,
      var color: Color) {
    
    var isDead: Boolean = false
    var isDying: Boolean = false
    var original: Boolean = true
    var isFlipped: Boolean = false
    
    //private[this] val colorF = 1
    //color *= colorF
    var newColor = color
    //var newNewColor = newColor

    var x = sx
    var y = sy

    private[this] val ssize = TableRandom.nextDouble*0.7+0.3
    
    // try this for a good time, also different sssizes
    //val xssize = x+ssize
    //val yssize = y+ssize

    def render() = {//(camx: Double, camy: Double, camw: Double, camh: Double): Unit = {
      glColor3d(color.r, color.g, color.b)
      //glColor4d(color.r/colorDiv, color.g/colorDiv, color.b/colorDiv, transparency)
      //val xx = ((x+camx)*camw/1280d).toInt
      //val yy = ((y+camx)*camh/720d).toInt

      glVertex2d(x,       y)
      glVertex2d(x+ssize, y)
      glVertex2d(x+ssize, y+ssize)
      glVertex2d(x,       y+ssize)
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
