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
import org.ljudmila.Utils.{ TableRandom, Tau, pow2, angleDist, angleAvg, angleAvgW, getRatio, withAlternative }
import GLadDOnS._
import org.ljudmila.hardware.Sound
import java.nio._
import java.lang.Double.parseDouble

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

      str
        .split("\n-+\n")
        .collect { case str if !str.trim.isEmpty =>
          OBJModel.apply(str, baseTransform, baseTransformVector)
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
            baseTransformVector + Transform(vec(withAlternative({val a = reader("speed"); println(a); a}, "0, 0, 0")) * -0.05),
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

      for (line <- file.getLines.buffered) {
        val x = line.split(" ")

        (line.charAt(0): @switch) match {
          case 'v' => (line.charAt(1): @switch) match {
            case ' ' => vertices  += Vec(parseDouble(x(1)), parseDouble(x(2)), parseDouble(x(3)))
            case 'n' => normals   += Vec(parseDouble(x(1)), parseDouble(x(2)), parseDouble(x(3)))
            case 't' => //uv/uvVertices += UV(x(1).toDouble, x(2).toDouble)
          }

          case 'f' =>
            faces += x.tail.map { face =>
              val fs = face.split("/")
              if (fs.length == 1)      (fs(0).toInt-1,            -1,            -1)
              else if (fs.length == 2) (fs(0).toInt-1, fs(1).toInt-1,            -1)
              else if (fs(1).isEmpty)  (fs(0).toInt-1,            -1, fs(2).toInt-1)
              else                     (fs(0).toInt-1, fs(1).toInt-1, fs(2).toInt-1)
            }

          case _ => // nop
        }
      }

      file.close

      (new RawModel(vertices.result(), /*//uv/uvVertices.result(),*/ normals.result(), faces.result())).toDisplayModel
    })

    def preload(files: Array[File], max: Int = -1): Unit = {
      (if (max == -1) files else files.take(max))
        .filterNot(file => modelCache.contains(file.toString))
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
    def random360: Vec = random01 * 360
    def random01: Vec = Vec(nextDouble, nextDouble, nextDouble)
    def randomGaussian: Vec = Vec(TableRandom.nextGaussian, TableRandom.nextGaussian, TableRandom.nextGaussian)
    def randomGaussian(d: Double): Vec = Vec(TableRandom.nextGaussian*d, TableRandom.nextGaussian*d, TableRandom.nextGaussian*d)
    def randomGaussianUnsafe(d: Double): Vec = Vec(TableRandom.nextGaussianUnsafe*d, TableRandom.nextGaussianUnsafe*d, TableRandom.nextGaussianUnsafe*d)
    def randomUniform01: Vec = { val rnd = nextDouble; Vec(rnd, rnd, rnd) }
  }
  case class Vec(x: Double, y: Double, z: Double) extends VecLike
  implicit def mutableVec(it: Vec): MutableVec = MutableVec(it.x, it.y, it.z)
  case class MutableVec(var x: Double, var y: Double, var z: Double) extends VecLike {
    def ++=(v: VecLike): Unit = { x += v.x; y += v.y; z += v.z; }
    def **=(f: Double): Unit = { x *= f; y *= f; z *= f }
    def fliptwo(times: Double): Unit = { x *= -times;  y *= -times }
    def flip(times: Double): Unit = { x *= -times; y *= -times; z *= -times; }
  }
  def vec(s: String): Vec = {
    if (s.isEmpty) vec0
    else {
      val split = s.split(" *, *")
      if (split.size == 1) vec(s.toDouble)
      else Vec(split(0).toDouble, split(1).toDouble, split(2).toDouble)
    }
  }
  val mutavec0: MutableVec = vec(0)
  def vec(d: Double): Vec = Vec(d, d, d)
  def vecx(d: Double): Vec = Vec(d, 0, 0)
  def vecy(d: Double): Vec = Vec(0, d, 0)
  def vecz(d: Double): Vec = Vec(0, 0, d)
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
  case class Transform(pos: Vec = vec0, rot: Vec = vec0, size: Vec = vec0) extends TransformLike
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
    def -(r: Rotation): Rotation = (
      Rotation(
          angleDist(yaw, r.yaw).toFloat,
          angleDist(pitch, r.pitch).toFloat,
          angleDist(roll,r.roll).toFloat)
    )
    def toC(a: Float, f: Float): Float = (
      if (a > 0+f) (a - f)
      else if (a < 0-f) (a + f)
      else 0f
    )
    def toCenter(f: Float): Rotation = (
      Rotation(
          toC(yaw, f),
          toC(pitch, f),
          toC(roll, f))
    )
    def avg(r: Rotation): Rotation = (
      Rotation(
          angleAvg(yaw, r.yaw).toFloat,
          angleAvg(pitch, r.pitch).toFloat,
          angleAvg(roll,r.roll).toFloat)
    )
    def avgW(r: Rotation, ratio: Double): Rotation = (
      Rotation(
          angleAvgW(yaw, r.yaw, ratio).toFloat,
          angleAvgW(pitch, r.pitch, ratio).toFloat,
          angleAvgW(roll,r.roll, ratio).toFloat)
    )
    def naiveMinus(r: Rotation): Rotation = Rotation(yaw-r.yaw, pitch-r.pitch, roll-r.roll)
    def *(f: Float): Rotation = Rotation(yaw*f, pitch*f, roll*f)
  }
  val rotation0 = Rotation(0, 0, 0)

  class Color(var r: Double, var g: Double, var b: Double) {
    def -=(f: Double): Unit = { r -= f; g -= f; b -= f }
    def *=(f: Double): Unit = { r *= f; g *= f; b *= f }

    def *(f: Double): Color = new Color(r * f, g * f, b * f)

    def toRGB(): Int =
      ((r*255).toInt) +
      ((g*255).toInt >> 8) +
      ((b*255).toInt >> 16)
  }
  object Color {
    def RGB(i: Int): Color = new Color((i & 255)/255d, ((i >> 8) & 255)/255d, ((i >> 16) & 255)/255d)
    def BGR(i: Int): Color = new Color(((i >> 16) & 255)/256d, ((i >> 8) & 255)/256d, (i & 255)/256d)
    def Gray(i: Int): Color = new Color((i & 255)/255d, (i & 255)/255d, (i & 255)/255d)
  }
  def color(s: String): Color = {
    if (s.isEmpty) grey0
    else {
      val split = s.split(" *, *")
      if (split.size == 1) grey(s.toDouble)
      else new Color(split(0).toDouble, split(1).toDouble, split(2).toDouble)
    }
  }
  def grey(d: Double): Color = new Color(d, d, d)
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
        phi: Double = Tau*TableRandom.nextDoubleUnsafe,
        theta: Double = 0,
        baseVector: Vec = vec0,
        coreTransform: MutableTransform = transform000): Model = {

      Model(displayList, transform, transformVector, tex, color, alpha, phi, theta, baseVector, coreTransform, spread)
    }
  }

  class RawModel(vertices: Vertices, /*//uv/uvVertices: UVVertices,*/ normals: Normals, faces: Faces) {
    // Compile model to display list for faster drawing
    def toDisplayModel: DisplayModel =
      new DisplayModel(glDisplayList {
        var n = -1
        for (f <- faces.sortBy(_.length)) {
          if (f.length != n || f.length >= 5) {
            if (n != -1) glEnd()
            n = f.length
            glBegin((n: @switch) match {
              case 1 => GL_POINTS
              case 2 => GL_LINES
              case 3 => GL_TRIANGLES
              case 4 => GL_QUADS
              case _ => GL_POLYGON
            })
          }

          for ((vi, vti, vni) <- f) {
            if (vni != -1) glNormal3d(normals(vni).x, normals(vni).y, normals(vni).z)
            //uv/if (vti != -1) glTexCoord2d(uvVertices(vti).u, uvVertices(vti).v)
            glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
          }
        }
        if (n != -1) glEnd()

        //wireframe experiment
        /*if (lines) for (f <- faces) {
          glColor3d(0, 0, 0)
          glPrimitive(GL_LINE_STRIP) {
            for ((vi, vti, vni) <- f) {
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
        glCapabilities(GL_DEPTH_TEST, GL_LIGHTING, GL_BLEND) {
          glTheUsualBlendFunc()
          if (tex != -1) {
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
  class RenderProcessData(
      val beat: Boolean = false,
      val pullMode: Boolean = false,
      var centerx: Double, var centery: Double,
      val ratio: Double)
  {
      val ratio1m = 1-ratio
  }

  class RenderRenderData(
      val camx: Double, val camy: Double,
      val camw: Double, val camh: Double,
      val extraSize: Double)
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
    var threadMap: Map[Line, Vector[Thread]] = _
    var allNodes: Vector[ThreadNode] = _
    var setPulling = false

    def apply(str: String): ThreadNetwork = {
      var initNodes = Vector.empty[Point]
      var nodes = Vector.empty[Point]
      var lines = Vector.empty[Line]
      var liminoidTexMap = Map.empty[Point, Int] withDefaultValue -1
      var danglingNodes = Vector.empty[Point]
      var phase = 0

      str.split('\n').map{ line =>
        line.replaceAll("[ \"]", "")
      }.filterNot{ line =>
        line.isEmpty || line.startsWith("#") || line.startsWith("//")
      }.foreach{ line =>
        if (line == "-") phase += 1
        else phase match {
          case 0 =>
            initNodes :+= Point(line.split(','))
          case 1 =>
            val strings = line.split(',')
            val node = Point(strings)
            nodes :+= node
            if (strings.length > 2) {
              liminoidTexMap += node -> Texture(strings(3))
            }
          case 2 =>
            danglingNodes :+= Point(line.split(','))
          case 3 =>
            try {
              var strings = line.split("->")
              var direction = 1
              if (strings.length == 1) {
                strings = line.split("<-")
                direction = -1
              }
              def parse(str: String) = {
                if (str(0) == 'i') initNodes(str.tail.toInt-1)
                else if (str(0) == 'd') danglingNodes(str.tail.toInt-1)
                else nodes(str.toInt-1)
              }


            lines :+=
              (if (direction == 1) {
                new Line(parse(strings(0)), parse(strings(1)))
              } else {
                new Line(parse(strings(1)), parse(strings(0)))
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
          case p => println(s"Unknown phase in thread network: $p")
        }
      }

      threadMap = lines.map(line => line -> Thread.generateMultiThread(2)(line)).toMap

      val threadNodes =
          nodes.map(node =>
            new ThreadNode(
              node,
              insNodes  = lines.filter(_.p2 eq node),
              outsNodes = lines.filter(_.p1 eq node),

              liminoidTexMap(node),
              20+nextInt(15)
            )
          )
        val initThreadNodes =
          initNodes.map(node =>
            new ThreadNode(
              node,
              insNodes  = Vector.empty,
              outsNodes = lines.filter(_.p1 eq node),
              liminoidTexMap(node),
              20+nextInt(15),
              InitNode
            )
          )
      val danglingThreadNodes =
          danglingNodes.map(node =>
            new ThreadNode(
              node,
              insNodes  = lines.filter(_.p2 eq node),
              outsNodes = lines.filter(_.p1 eq node),
              -1,
              20+nextInt(15),
              DanglingNode
            )
          )

      new ThreadNetwork(initThreadNodes, threadNodes, danglingThreadNodes, lines, threadMap)
    }
  }

  class ThreadNetwork(
      val initNodes: Vector[ThreadNode],
      val nodes: Vector[ThreadNode],
      val danglingNodes: Vector[ThreadNode],
      val lines: Vector[Line],
      val threadMap: Map[Line, Vector[Thread]]) extends RenderObject {

    ThreadNetwork.allNodes = initNodes ++ nodes ++ danglingNodes

    var fullyOver = false

    def fullyVisible: Boolean =
      nodes.forall(_.fullyVisible) &&
      danglingNodes.forall(_.fullyVisible)

    def totallyVisibleNodes: Boolean =
      nodes.forall(_.totallyVisible)

    def totallyVisible: Boolean =
      nodes.forall(_.totallyVisible) && danglingNodes.forall(_.totallyVisible)

    def init(): Unit = {
      for (node <- nodes) {
        node.init()
      }

      //val noparentThreads = nodes.flatMap(_.ins).toSet &~ nodes.flatMap(_.outs).toSet;
      for (node <- initNodes) {
        node.init()
        node.outs.foreach(_.init())
      }
    }

    var almostOver = false
    def process(implicit data: RenderProcessData): Unit = {
      if (data.pullMode) {
        data.centerx = danglingNodes.head.position.x
        data.centery = danglingNodes.head.position.y
        if (ThreadNetwork.setPulling) {
          ThreadNetwork.setPulling = false
          val nodess = nodes.filterNot(_.pulling)
          if (nodess.nonEmpty) {
            val nn = nodess.minBy(node => -sqrt(pow2(node.position.x - data.centerx) + pow2(node.position.y - data.centery)))

            nn.pulling = true
            ThreadNetwork.allNodes
              .filter(_.nodeType == DanglingNode)
              .find(dn => dn.ins.exists(t => nn.outs contains t))
              .foreach(_.pulling = true)
          } else {
            almostOver = true
            println("Almost over")
          }
        }
        if (!fullyOver && almostOver) {
          val maxdistReached = nodes.forall(node =>
            abs(node.position.x - data.centerx) +
            abs(node.position.y - data.centery) < 12
          )
          if (maxdistReached) {
            println("It's over")
            fullyOver = true
          }
        }
        //danglingNodes.foreach(_.pulling = true)
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

  class ThreadNode(
      val position: Point,
      val insNodes: Vector[Line],
      val outsNodes: Vector[Line],
      val texture: Int,
      val nodeSize: Double,
      val nodeType: ThreadNodeType = RegularNode) extends RenderObject {

    val ins  = insNodes.flatMap(ThreadNetwork.threadMap)
    val outs = outsNodes.flatMap(ThreadNetwork.threadMap)

    var outsInitialized = false
    var pulled: Boolean = false
    var pulling: Boolean = false
    var scheduledPulling: Boolean = false

    def visible: Double =
      if (ins.isEmpty) 0d else min(1d, ins.map(thread => thread.backupvisible()).max)

    var ownvisible = 1d

    var exvisible = 0d
    val invisibleThreshold = 0.05
    def wasInvisible: Boolean = {
      val out = (exvisible <= invisibleThreshold) && (visible > invisibleThreshold)
      exvisible = visible
      out
    }

    def fullyVisible: Boolean =
      ins.isEmpty || ins.maxBy(_.backupvisible()).backupvisible() >= 1d // maxBy, not exists because of fade side effects

    def totallyVisible: Boolean =
      ins.forall(_.backupvisible() >= 1d)

    def init(): Unit = {
      //
    }

    def process(implicit data: RenderProcessData): Unit = {
      if (fullyVisible) {
        if (!outsInitialized) {
          println("Outs initialized")
          outsInitialized = true
          for (out <- outs) out.init()
        }
        for (out <- outs) {
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
          ThreadNetwork.allNodes.foreach {
            x => x.scheduledPulling = true
          }
        }
      }
      /*if (wasInvisible && nodeType == RegularNode) {
        val soundPan =
          if (position.x > data.centerx+250) "R"
          else if (position.x < data.centerx-250) "L"
          else ""

        Sound.play("network"+soundPan+(nextInt(6)+1));
      }*/
      if (abs(position.x - data.centerx) + abs(position.y - data.centery) < 20) {
        ownvisible -= 0.01
      }
      //for (in <- ins) if (in.isInitialized) in.process
    }

    def render(implicit data: RenderRenderData): Unit = {
      renderThreads
      renderNode
    }
    def renderThreads(implicit data: RenderRenderData): Unit = {
      data.pullingThisThread = pulling
      glCapability(GL_BLEND) {
        glTheUsualBlendFunc()
        render2D {
          glPushMatrix()
          glTranslated(data.camx, data.camy, 0)
          glScaled(data.camw/1920d, data.camh/1080d, 1)
          for (in <- ins) in.render
          glPopMatrix()
        }
      }
    }
    def renderNode(implicit data: RenderRenderData): Unit = {
      val visibility = if (pulled) ownvisible else if (pulling) 1 else visible*0.8+0.2
      val liminoidSizex = position.s*visibility //+ (TableRandom.nextGaussianUnsafe/100d) * position.s
      val liminoidSizey = position.s*visibility //+ (TableRandom.nextGaussianUnsafe/100d) * position.s
      val posx = position.x-liminoidSizex/2 + (TableRandom.nextGaussianUnsafe/50d) * position.s
      val posy = position.y-liminoidSizey/2 + (TableRandom.nextGaussianUnsafe/50d) * position.s

      val coords = Coord(posx, posy, liminoidSizex, liminoidSizey) + (data.extraSize*10)
      quad(coords, texture, flipx = false, flipy = false, if (pulled) ownvisible else if (pulling) 1 else visible*2,
        preRender = {
          glPushMatrix()
          glTranslated(data.camx, data.camy, 0)
          glScaled(data.camw/1920d, data.camh/1080d, 1)
        },
        postRender = {
          glPopMatrix()
        })
    }
  }

  object Point {
    def apply(str: Array[String]): Point = {
      if (str.length == 2)
        new Point(str(0).toDouble+Liminoid.threadNetworkOffsetx, str(1).toDouble+Liminoid.threadNetworkOffsety)
      else
        new Point(str(0).toDouble+Liminoid.threadNetworkOffsetx, str(1).toDouble+Liminoid.threadNetworkOffsety, str(2).toDouble)
    }
  }
  class Point(var x: Double, var y: Double, var s: Double = 1.0) {
    s = s*30
    def toTuple: (Double, Double) = (x, y)
    //def toTuple() = (x, y, s)
  }
  object ThreadPoint {
    val visibilityThreshold = 0.00035d
    val visibilityVelocity = 0.0055d
  }
  class ThreadPoint(var desiredx: Double, var desiredy: Double) {
    var x: Double = desiredx
    var y: Double = desiredy
    var i: Double = 0.5d
    var iv: Double = 0d
    var ivv: Double = 0d
    var visible: Double = 0d
    var visiblev: Double = 0d
    var ratio = 0.1
    var paused = false

    def isVisible(): Boolean = visible > ThreadPoint.visibilityThreshold
    def fullyVisible(): Boolean = visible >= 1d

    def init(): Unit = {
      visiblev = ThreadPoint.visibilityVelocity
      visible = ThreadPoint.visibilityThreshold + visiblev
    }

    var children: Vector[ThreadPoint] = Vector.empty

    //def dist(that: ThreadPoint): Double = sqrt(pow2(this.x-that.x) + pow2(this.y-that.y))
  }

  object Line {
    def apply(a: Array[String]): Line =
      new Line(
        new Point(a(0).toDouble, a(1).toDouble),
        new Point(a(2).toDouble, a(3).toDouble))
  }
  class Line(val p1: Point, val p2: Point) {
    def line: Vector[Point] = Vector(p1, p2)
  }

  object Thread {
    def generateMultiThread(n: Int)(l: Line): Vector[Thread] = {
      Vector.fill(n)(generateThread(l))
    }
    def generateThread(l: Line): Thread = {
      val thread =
        new Thread({
          val (sx, sy) = l.p1.toTuple
          val (dx, dy) = l.p2.toTuple
          val segments = max(5, math.hypot(sx-dx, sy-dy)/39d)
          val segmentsInt = segments.toInt
          var prev: Option[ThreadPoint] = None
          Vector.tabulate(segmentsInt) { i =>
            val (ratio1, ratio2) = getRatio(1 - i/segments)
            val out = (
              if (i <= 0) {
                new ThreadPoint(sx, sy)
              } else if (i >= segmentsInt-1) {
                new ThreadPoint(dx, dy)
              } else {
                new ThreadPoint(
                    sx*ratio1+dx*ratio2 + TableRandom.nextGaussianUnsafe/7d,
                    sy*ratio1+dy*ratio2 + TableRandom.nextGaussianUnsafe/7d)
              }
            )

            val children = Vector(out)
            prev.foreach(_.children = children)

            prev = Some(out)
            out
          }
        })

      thread.init()
      thread
    }
  }
  class Thread(nodes: Vector[ThreadPoint]) {
    var currentLength = 1

    def nodesLast: ThreadPoint = nodes.last//(nodes.size-2)

    var fejd = 0.4

    def backupvisible(): Double =
      if (currentLength < nodes.size || nodesLast.visible < 1.5) 0
      else {
        fejd = fejd * 0.97 + 1*0.03
        if (fejd > 0.95) fejd = 1
        fejd
      }

    //def fullyVisible(): Boolean = nodesLast.fullyVisible

    var isInitialized = false
    def init(): Unit = {
      isInitialized = true
      nodes.head.init()
    }

    def process(implicit data: RenderProcessData): Unit = {
      var i = 0
      do {
        val node = nodes(i)
        node.x = node.desiredx*(node.ratio) + node.x*(1 - node.ratio) + TableRandom.nextGaussianUnsafe/1.5d
        node.y = node.desiredy*(node.ratio) + node.y*(1 - node.ratio) + TableRandom.nextGaussianUnsafe/1.5d
        if (i > 0) {
          val prev = nodes(i-1)
          val dx = node.desiredx-node.x
          val dy = node.desiredy-node.y
          prev.x = prev.x + dx/10d
          prev.y = prev.y + dy/10d
        }
        node.i += node.iv
        node.iv += node.ivv
        node.visible += node.visiblev
        if (node.i > 0.9) node.ivv = -0.01
        else if (node.i < 0.75) node.ivv = +0.01

        if (node.i < 0.6) node.i = 0.6

        /*if (i < nodes.length) {
          if (nodes(i-1) dist node > 5) {
            //TODO: point it back or add nodes
          }
        }*/

        i += 1
      } while (i < currentLength && !nodes(i).paused)
      if (data.beat && i < nodes.length && (i >= currentLength || nodes(i).paused)) {
        if (nodes(i).paused) {
          if (nodes(i-1).visible > 0.9) {
            nodes(i).paused = false
          }
        } else {
          // Init next node
          currentLength += 1
          if (currentLength > nodes.length) currentLength = nodes.length
          nodes(i).init()
        }
      }
      // lame scaling pull
      /*if (data.pullMode) {
        for (node <- nodes) {
          node.x = node.x*data.ratio + data.centerx*data.ratio1m
          node.y = node.y*data.ratio + data.centery*data.ratio1m
          node.desiredx = node.desiredx*data.ratio + data.centerx*data.ratio1m
          node.desiredy = node.desiredy*data.ratio + data.centery*data.ratio1m
        }
      }*/
    }

    def render(implicit data: RenderRenderData): Unit = {
      glLineWidth((2d /*+ (testNum2/10d)*(osc1+1)*/).toFloat)
      glPrimitive(GL_LINE_STRIP) {
      //glPrimitive(GL_LINES) {// Dashed
        var i = 0
        while (i < nodes.length && nodes(i).isVisible) {
          val node = nodes(i)
          if (node.y < 0-120 || node.y > 1080+120) {
            //nop
          } else if (i > 0 && (node.visible < 1d || data.pullingThisThread)) {
            val prevNode = nodes(i-1)
            if (data.pullingThisThread) {
              node.visible = node.visible * 0.9
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

  class Pixel(
      var sx: Double, var sy: Double,
      var transformVector: MutableVec = mutavec0,
      var color: Color) {

    var isDead: Boolean = false
    var isDying: Boolean = false
    var isFlipped: Boolean = false
    var isContained: Boolean = true

    var newColor = color

    var x = sx
    var y = sy

    //private[this] val ssize = TableRandom.nextDoubleUnsafe+0.45
    private[this] val ssize = TableRandom.nextDoubleUnsafe*0.6+0.8

    def render(): Unit = {
      val rat = 2d//+testNum1/100d
      glColor3d(color.r*rat, color.g*rat, color.b*rat)
      glVertex2d(x,       y)
      glVertex2d(x+ssize, y)
      glVertex2d(x+ssize, y+ssize)
      glVertex2d(x,       y+ssize)
    }
  }

  class NoisePixel(
      var sx: Double, var sy: Double,
      val transformVector: MutableVec = mutavec0,
      private val color: Double) {

    var x = sx
    var y = sy

    private[this] val ssize = TableRandom.nextDoubleUnsafe*0.4+0.5

    def render(): Unit = {
      glColor3d(color, color, color)
      val xx = x+ssize
      val yy = y+ssize
      glVertex2d(x,  y)
      glVertex2d(xx, y)
      glVertex2d(xx, yy)
      glVertex2d(x,  yy)
    }
  }

  case class Coord(x: Double, y: Double, w: Double, h: Double) {
    def +(d: Double): Coord = {
      val dH = d/2
      Coord(x - dH, y - dH, w + d, h + d)
    }
  }

  def quad(
      coord: Coord, texture: Int = -1,
      flipx: Boolean = false, flipy: Boolean = false,
      alpha: Double = 1d, color: Color = grey1, blend: (Int, Int) = (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA),
      preRender: => Unit = {},
      postRender: => Unit = {}): Unit = {

    glDisable(GL_DEPTH_TEST)
    glDisable(GL_LIGHTING)


    if (blend != ((-1, -1))) {
      glEnable(GL_BLEND)
      glBlendFunc(blend._1, blend._2)
    } else {
      glEnable(GL_ALPHA_TEST)
      glAlphaFunc(GL_GEQUAL, 0.9f)
    }

    if (texture != -1) {
      glEnable(GL_TEXTURE_2D)
      glBindTexture(GL_TEXTURE_2D, texture)
    }


    glColor4d(color.r, color.g, color.b, alpha)

    val (v0, v1) = if (flipy) (0f, 1f) else (1f, 0f)
    val (h0, h1) = if (flipx) (0f, 1f) else (1f, 0f)

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

    if (texture != -1) glDisable(GL_TEXTURE_2D)

    if (blend != ((-1, -1))) {
      glDisable(GL_BLEND)
    } else {
      glDisable(GL_ALPHA_TEST)
    }

    glEnable(GL_LIGHTING)
    glEnable(GL_DEPTH_TEST)
  }
}
