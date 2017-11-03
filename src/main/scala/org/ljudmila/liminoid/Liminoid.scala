package org.ljudmila.liminoid

import org.lwjgl.opengl.{ Display, PixelFormat, DisplayMode, Util }
import org.lwjgl.input.{ Keyboard, Mouse }
import collection.mutable
import collection.parallel.mutable.ParArray
import java.nio._
import scala.util.Random._
import math._
import org.ljudmila.Utils
import org.ljudmila.Utils._
import org.ljudmila.SettingsReader
import org.ljudmila.hardware
import org.ljudmila.hardware.{ RiftTracker, PulseSensor, Sound }
import org.lwjgl.opengl.{ GL11, GL12, GL13, GL14 }
import org.lwjgl.opengl.GL11._
import scala.annotation.switch
import Thread.sleep

import Render.{ render3D, render2D }
import GLadDOnS._

import Models.{ Transform, OBJModel, OBJModels, quad, Coord, Point, Model, Pixel, NoisePixel }
import Models.{ Color, grey, grey0, grey1 }
import Models.{ Vec, vec, vecx, vecy, vecz, vec0, vec05, vec1, vec2, vec3, vec4, vec5, vec90x }
import Models.{ Rotation, rotation0 }
import Models.{ Thread, ThreadNetwork, ThreadNode, Line, RenderProcessData, RenderRenderData }

final object Liminoid {
  val project = "Liminoid"
  val settings = SettingsReader.load("Settings.txt")
  val dataFolder = settings("dataFolder")
  var startLiminoid = settings("autostart").equalsIgnoreCase("true")

  var renderMode = RenderMode(settings("renderMode"))

  var isMainLoopRunning = false
  var renderTime = 0d
  var pause = false

  /**
   * Initializes display and enters main loop
   */
  def main(args: Array[String]): Unit = {
    initDisplay()
    PulseSensor.init()
    RiftTracker.init()
    Sound.init(settings("snds"))

    mainLoop()

    // Cleanup
    Sound.stopAll()
    RiftTracker.destroy()
    PulseSensor.close()
    Display.destroy()
  }

  val (winWidth, winHeight) = (settings("screenx").toInt, settings("screeny").toInt)
  val (forceWidth, forceHeight) = (settings("screenx").toInt, settings("screeny").toInt)
  def initDisplay(): Unit = {
    Display.setTitle(project)

    /*val bestMode =
      Display
        .getAvailableDisplayModes
        .filter(mode => /*mode.isFullscreenCapable &&*/ mode.getWidth == forceWidth && mode.getHeight == forceHeight)
        .maxBy(_.getBitsPerPixel)*/
    val bestMode = new DisplayMode(1280, 800)

    Display.setVSyncEnabled(true)
    Display.setFullscreen(true)
    Display.setDisplayModeAndFullscreen(bestMode)
    Display.setVSyncEnabled(true)
    Display.setFullscreen(true)
    //winWidth = bestMode.getWidth
    //winHeight = bestMode.getHeight

    println("Display: "+bestMode.getWidth+"x"+bestMode.getHeight+"@"+bestMode.getFrequency+"Hz, "+bestMode.getBitsPerPixel+"bit")

    Display.create()
    //TODO: try antialiasing/multisampling/...:
    //Display.create(new PixelFormat(24, 8, 16, 8, 8))
    //Display.create(new PixelFormat(8, 0, 0, 8))

    Mouse.setGrabbed(true)
  }

  // Frame-independent movement timer
  var frameTime = currentTime

  /**
   * Game loop: renders and processes input events
   */
  def mainLoop(): Unit = {
    setupView()  // setup camera and lights
    val shader = new RiftShader(winWidth, winHeight)

    // FPS counter
    var frameCounter = 0
    val second = 1000000000L // ns -> s
    val FPSseconds = 2
    var FPStimer = currentTime
    frameTime = currentTime

    isMainLoopRunning = true
    while (isMainLoopRunning) {
      processInput() // process keyboard input
      resetView()    // clear view and reset transformations

      renderMode match {
        case Mono =>
          renderFrame()    // draw stuff
          Display.update() // update window contents and process input messages
        case Stereo =>
          shader.beginOffScreenRenderPass()
          try {
            renderFrame()    // draw stuff
          } catch {
            case e: Throwable =>
          }
          shader.endOffScreenRenderPass()
          shader.renderToScreen()
          Display.update() // update window contents and process input messages
      }

      frameCounter += 1

      if (currentTime-FPStimer > second*FPSseconds) {
        val FPS = frameCounter/FPSseconds

        println("FPS: "+FPS)
        println("sinceStart: "+since(phaseTimer))
        if (testNum1 != 0) println("testNum1: "+testNum1)
        if (testNum2 != 0) println("testNum2: "+testNum2)
        if (testNum3 != 0) println("testNum3: "+testNum3)
        if (testNum4 != 0) println("testNum4: "+testNum4)
        if (testNum5 != 0) println("testNum5: "+testNum5)
        if (testNum6 != 0) println("testNum6: "+testNum6)
        if (currentPhase == Radiolarians) println("radiolarian: "+radiolarian.transform)
        println("rotation:"+rotation)
        //println("Particles: "+backPixels.size)

        frameCounter = 0
        FPStimer = currentTime
      }

      renderTime = (currentTime-frameTime) / 2.5E7

      frameTime = currentTime
    }
  }

  /**
  * Initial setup of projection of the scene onto screen, lights etc.
  */
  def setupView(): Unit = {
    glClearColor(0, 0, 0, 0)

    glEnable(GL_DEPTH_TEST) // enable depth buffer (off by default)
    //glEnable(GL_CULL_FACE)  // enable culling of back sides of polygons
    //glCullFace(GL_BACK)

    // smooth shading - Gouraud
    glShadeModel(GL_SMOOTH)
    //glShadeModel(GL_FLAT)
    glHint(GL_POINT_SMOOTH_HINT, GL_NICEST)
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)
    //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST)
    glEnable(GL_POINT_SMOOTH)
    glEnable(GL_LINE_SMOOTH)
    //glEnable(GL_POLYGON_SMOOTH)

    setupLights()

    glViewport(0,0, winWidth,winHeight) // mapping from normalized to window coordinates
  }

  def setupLights(): Unit = {
    // lights
    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)

    // LWJGL makes float buffers a bit difficult
    def floatBuffer(a: Float*): FloatBuffer = (
      ByteBuffer
        .allocateDirect(a.length*4)
        .order(ByteOrder.nativeOrder)
        .asFloatBuffer
        .put(a.toArray)
        .flip
        .asInstanceOf[FloatBuffer]
    )

    val mod1 = 0//testNum1/100f
    val mod2 = 0//testNum2/100f
    val mod3 = 0//testNum3/100f
    val mod4 = 0//testNum4/100f
    glLight(GL_LIGHT0, GL_AMBIENT, floatBuffer(0.65f+mod1, 0.65f+mod1, 0.65f+mod1, mod3))
    glLight(GL_LIGHT0, GL_DIFFUSE, floatBuffer(0.7f+mod2, 0.7f+mod2, 0.7f+mod2, mod4))
    glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 20f)
    glLight(GL_LIGHT0, GL_POSITION, floatBuffer(1f, 1f, 0, 0f))
    glEnable(GL_COLOR_MATERIAL)
    glMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, floatBuffer(0.9f+mod3, 0.9f+mod3, 0.9f+mod3, 0f))
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
  }

  /**
  * Resets the view of current frame
  */
  def resetView(): Unit = {
    //TODO: clear color and depth buffer?
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
  }

  /**
  * Renders current frame
  */

  lazy val liminoidSplash = Texture(settings("liminoidSplash"))
  lazy val liminoidSplashLoading = Texture(settings("liminoidSplashLoading"))

  /// Liminoid phases ///
  val phases =
    mutable.ListMap(
      "Limbo" -> -1,
      "Setup" -> 0,
      "Radiolarians" -> 1,
      "Mandalas" -> 2,
      "Schizoid" -> 3,
      "Threads" -> 4,
      "FlyingRock" -> 5
    )
  val Limbo        = phases("Limbo")
  val Setup        = phases("Setup")
  val Radiolarians = phases("Radiolarians")
  val Mandalas     = phases("Mandalas")
  val Schizoid     = phases("Schizoid")
  val Threads      = phases("Threads")
  val FlyingRock   = phases("FlyingRock")
  val PhaseTerminator = phases.last._2 // Last phase
  var phaseTimer = now // Tracks time from the beginning of phase

  var currentPhase = Setup // Current phase
  var phaseChanged = true

  def gotoPhase(i: Int): Unit = {
    currentPhase = i
    phaseChanged = true
  }
  def nextPhase(): Unit = {
    if (phaseChanged || currentPhase >= PhaseTerminator) return
    currentPhase += 1
    phaseChanged = true
  }
  def previousPhase(): Unit = {
    if (phaseChanged || currentPhase <= Setup) return
    currentPhase -= 1
    phaseChanged = true
  }
  def initPhase(initFunc: => Unit): Unit =
    if (phaseChanged) {
      phaseTimer = now
      phaseChanged = false
      initFunc
      System.gc() // best place to do it...
    }

  // http://www.dspguru.com/dsp/howtos/how-to-create-oscillators-in-software
  // f = desired oscillator frequency
  // w = 2 pi f / Fs
  // phi = initial phase
  // y = output array containing the oscillator signal
  // def oscillator(i: Double) = sin((2*Pi*f)*i + phi)
  def oscillator(i: Double = Utils.now*0.002, phi: Double): Double = sin(i + phi)
  var osc1 = 0d
  var osc2 = 0d
  var osc3 = 0d
  var osc4 = 0d

  // variables that go up to 1 by little each frame
  var (fade1, fadeSpeed1) = (1d, 0.002)
  var (fade12, fadeSpeed12) = (1d, 0.01)
  var (fade2, fadeSpeed2) = (1d, 0.04)
  var (fade22, fadeSpeed22) = (1d, 0.005)
  var (fadeFlash, fadeSpeedFlash) = (1d, 0.06)
  var (fadeSlow, fadeSpeedSlow) = (1d, 0.001)

  // Cameras
  //val cams = Array(hardware.Camera(camId = 0, 1920, 1080), hardware.Camera(camId = 1, 1920, 1080), hardware.Camera(camId = 2, 1280, 720), hardware.Camera(camId = 3, 1280, 720))
  //val stereoCameras = cams.takeRight(2).reverse

  //val backCamera = hardware.Camera(camId = 1, 1920, 1080) //cams(0)
  //val frontCamera = hardware.Camera(camId = 0, 1280, 720) //960?
  //val flipFrontCamera = true

  //val backCamera = hardware.Camera(camId = 0, 1280, 960) //cams(0)
  //val frontCamera = hardware.Camera(camId = 1, 1920, 1080)

  val frontCamera = hardware.Camera(camId = settings("frontCameraId").takeRight(1).toInt, settings("frontCameraX").toInt, settings("frontCameraY").toInt)
  val backCamera  = hardware.Camera(camId = settings("backCameraId").takeRight(1).toInt, settings("backCameraX").toInt, settings("backCameraY").toInt)

  val flipFrontCamera = true//false

  val eyeCorrection = -64 // Eye shift for 2D rift view
  var testNum1, testNum2, testNum3, testNum4, testNum5, testNum6 = 0 // Numbers for testing
  var exTestSum, testSum = 0

  val winCoord = Coord(0,0, winWidth,winHeight)
  val coord2000 = Coord(0,0, 2000,2000)
  /// Radiolarians phase objects ///
  val whiteish = grey(settings("whiteish").toDouble)
  val blackish = grey(settings("blackish").toDouble)

  val wallZ = settings("wallz").toInt // z position of wall
  val radioBasePosVecZ = -0.11 * 1.4 // 2min8s -> 1min30s (85x + 43x*1.75 = ...)
  val radioBasePosVec = Vec(0, 0, radioBasePosVecZ) // basic z movement vector
  val startPos = vecz(wallZ+15) // starting point for radiolarians
  def basicRot: Vec = Vec.randomGaussian(1d/3d)

  // The rock inside radiolarians
  lazy val core =
    OBJModel.load(settings("coreModel")).toModel(color = grey(0.05))

  // The radiolarian that opens up
  lazy val radiolarian =
    OBJSequence(
      path = settings("radiolarianFolder"),
      active = false,
      stopAtEnd = true,
      delay = 80,
      color = whiteish,
      coreTransform = Transform(pos = vec3),
      transform = Transform(pos = startPos + vecz(-5), rot = vec90x),
      transformVector = Transform(pos = radioBasePosVec, rot = vec0))

  // Some rocks just floating around
  lazy val _whiteRocks =
    OBJModels(
        settings("rocks1"),
        Transform(pos = startPos),
        Transform(pos = radioBasePosVec, rot = basicRot))
    //OBJModels(
    //    settings("rocks2"),
    //    Transform(pos = startPos),
    //    Transform(pos = radioBasePosVec, rot = basicRot)) ++
  lazy val _rocks = (
    OBJModels(
        settings("rocks3"),
        Transform(pos = startPos),
        Transform(pos = radioBasePosVec, rot = basicRot)) ++
    OBJModels(
        settings("rocks35"),
        Transform(pos = startPos),
        Transform(pos = radioBasePosVec, rot = basicRot)) ++
    OBJModels(
        settings("rocks4"),
        Transform(pos = startPos),
        Transform(pos = radioBasePosVec, rot = basicRot)) ++
    OBJModels(
        settings("rocks45"),
        Transform(pos = startPos),
        Transform(pos = radioBasePosVec, rot = basicRot))
  )
  var filteredRocks: Array[Model] = _

  /// Mandalas phase objects ///
  val startFrameNum = 2162 // filenames for mandalas start with this
  var blackMandala =
    TexSequence(
      settings("blackMandala"),
      delay = 24.FPS,
      selfDestruct = true)
  var whiteMandala =
    TexSequence(
      settings("whiteMandala"),
      delay = 24.FPS * 0.8,
      selfDestruct = true)
  var whiteFlashTimer = -1
  var startHeart = -1
  var startDustHeart = -1
  lazy val blackHeartMandala     = Texture(settings("blackHeartMandala"))
  lazy val blackHeartDustMandala = Texture(settings("blackHeartDustMandala"))
  lazy val whiteHeartMandala     = Texture(settings("whiteHeartMandala"))
  var zoom = 0d

  // Schizoid phase objects
  var wallTex = -1
  val back0Seq =
    TexSequence(
      settings("back0"),
      delay = 1000 / 15d,
      stopAtEnd = false,
      bounce = true,
      snap = Texture.getImage(settings("back0back") + "1.png"))
  val back1Seq =
    TexSequence(
      settings("back1"),
      delay = 1000 / 15d,
      stopAtEnd = false,
      bounce = true,
      snap = Texture.getImage(settings("back1") + "1.png"))
  val back2Seq =
    TexSequence(
      settings("back2"),
      delay = 1000 / 15d,
      stopAtEnd = false,
      bounce = true,
      snap = Texture.getImage(settings("back2") + "1.png"))
  //var backCamSnapTex = backCamera.getTextureIDWait
  var noisePixels = Vector.empty[NoisePixel]
  var backPixels = Vector.empty[Pixel]
  var backpixelBuffer = Array[Array[Boolean]]()
  var backPixelDrop = false
  var backPixelMerge = false
  var backPixelMerged = true
  var noiseWall = false
  var finished = false
  var diffStarted, diffDone = false
  var bpCenterx, bpCentery = 0d

  var networkStarted = false

  var rockfly = false

  var csize = 0.2d

  var shouldFreezeview = false
  def viewFrozen: Boolean = freezeView >= 0.01
  var freezeView = 0d

  def commonOffsetx: Double = settings("commonOffsetx").toDouble// + testNum1
  def commonOffsety: Double = settings("commonOffsety").toDouble// + testNum2

  def networkOffsetx: Double = settings("networkOffsetx").toDouble + commonOffsetx
  def networkOffsety: Double = settings("networkOffsety").toDouble + commonOffsety

  def threadNetworkOffsetx: Double = settings("threadNetworkOffsetx").toDouble + commonOffsetx
  def threadNetworkOffsety: Double = settings("threadNetworkOffsety").toDouble + commonOffsety

  var debugPoly = false
  val (pts, polygon, bound, pixelbox, pixelboxpoly) = {
        val b = """
939, 410
859, 450
760, 580
740, 700
747, 843
1040, 385
1120, 395
1190, 410
1290, 480
1340, 549
1380, 685
1365, 862
""".trim

        type AWTPoint = java.awt.Point

        def watx(a: Double) = a*1280/1920d + settings("noiseWallOffsetx").toDouble
        def waty(a: Double) = a*720/1080d + settings("noiseWallOffsety").toDouble

        val pts = b.trim.split('\n').foldLeft(List[java.awt.Point]()){ (acc, n) =>
          val spl = n.split(" *, *")
          //glTranslated(camx+testNum1, camy+testNum2, 0)
          //glScaled(camw/camresx, camh/camresy, 1)
          val xx = spl(0).toInt*1280/1920d + settings("noiseWallOffsetx").toDouble
          val yy = spl(1).toInt*720/1080d  + settings("noiseWallOffsety").toDouble
          //val x = ((xx+camx)*camw/1920d).toInt
          //val y = ((yy+camy)*camh/1080d).toInt
          //val x = ((xx*camw/1920d+camx)).toInt
          //val y = ((yy*camh/1080d+camy)).toInt
          (new java.awt.Point((xx+threadNetworkOffsetx).toInt, (yy+threadNetworkOffsety).toInt)) :: acc
        }

        object Point {
          def convexHull(_points: Seq[AWTPoint]): Seq[AWTPoint] = {
            if (_points.isEmpty) return _points
            val points = _points.sortBy(_.x)
            val upper = halfHull(points)
            val lower = halfHull(points.reverse)
            upper.remove(0)
            lower.remove(0)
            upper ++: lower
          }

          private def halfHull(points: Seq[AWTPoint]) = {
            val upper = new mutable.ListBuffer[AWTPoint]()
            for (p <- points) {
              while (upper.size >= 2 && leftTurn(p, upper(0), upper(1))) {
                upper.remove(0)
              }
              upper.prepend(p)
            }
            upper
          }

          private def leftTurn(p1: AWTPoint, p2: AWTPoint, p3: AWTPoint) = {
            val slope = (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
            val collinear = math.abs(slope) <= 1e-9
            val leftTurn = slope < 0
            collinear || leftTurn
          }
        }

        val p = new java.awt.Polygon()
        val hull = Point.convexHull(pts)
        for (pt <- hull) p.addPoint(pt.x, pt.y)

        val px1 = (watx(850) + threadNetworkOffsetx).toInt
        val px2 = (watx(1250) + threadNetworkOffsetx).toInt
        val py1 = (waty(550) + threadNetworkOffsety).toInt
        val py2 = (waty(850) + threadNetworkOffsety).toInt

        val pxx1 = (watx(950) + threadNetworkOffsetx).toInt
        val pxx2 = (watx(1150) + threadNetworkOffsetx).toInt
        val pyy1 = (waty(450) + threadNetworkOffsety).toInt

        val boxpts =  List(
          new java.awt.Point(px1, py1),
          new java.awt.Point(px2, py1),
          new java.awt.Point(px2, py2),
          new java.awt.Point(px1, py2),

          new java.awt.Point(pxx1, pyy1),
          new java.awt.Point(pxx2, pyy1)
        )
        val hullbox = Point.convexHull(boxpts)
        val p2 = new java.awt.Polygon()
        for (pt <- hullbox) p2.addPoint(pt.x, pt.y)

        /*def rectangleToPolygon(rect: java.awt.Rectangle) = {
          val result = new java.awt.Polygon()
          result.addPoint(rect.x, rect.y)
          result.addPoint(rect.x + rect.width, rect.y)
          result.addPoint(rect.x + rect.width, rect.y + rect.height)
          result.addPoint(rect.x, rect.y + rect.height)
          result
        }*/

        (hull, p, p.getBounds, hullbox, p2)
  }


  var noiseFrames = 0
  var wallTimer = 0
  var ballSize = 70d
  var spreadBall = false
  var spreadBallComplete = false

  var rockOut = false
  var tupRockEnabled = false
  var tupRock = 0
  def tupRockDisabled: Boolean = tupRock > 10



  var bpAreDying = false

  lazy val threadNetwork = ThreadNetwork(settings("threadNetwork"))

  // have a small bump for movement
  val shakeBump = 3
  val shakeBumpN = 50

  // Oculus rift head tracking
  var rotation, prevRotation = rotation0
  var rotation3d, prevRotation3d = rotation0

  var prevIzStene = false
  var prevTresenje = false
  var prevRadioVector = false
  var prevRadioOpen = false
  var prevEnd = false

  def drawFrontCam(): Unit = {
    val img = frontCamera.getTextureID
    val camScale = -1030 /*+ testNum5*/ + 427 + 145
    val camScaleH = camScale/2
    val camhCorrect = 650 /*+ testNum4*/ - 229 - 149
    val (camw, camh) = (winWidth, (winWidth*(9d/16d))+camhCorrect)
    val (camx, camy) = (winWidth/2-camw/2, -camhCorrect/2)
    quad(Coord(camx-camScaleH,camy-camScaleH, camw+camScale,camh+camScale), img, flipy = flipFrontCamera, flipx = true)
  }

  var schizoidPhase = 1

  /*val backBlend = (GL_ONE, GL_ONE)
  val backBlendColor = new Color(0, 0.08, 0.18)
  def backBlendRender(): Unit = {
    GL14.glBlendEquation(GL14.GL_FUNC_REVERSE_SUBTRACT)
    val alpha = if (currentPhase == Radiolarians) 0.07 else 0.1
    quad(winCoord, color = backBlendColor, alpha = alpha, blend = backBlend)
    GL14.glBlendEquation(GL14.GL_FUNC_ADD)
  }*/

  /// of the dissintegration
  var middlemoment = false


  var tuptupcounter = 0
  var tuptupcounted = false
  var tuptupenabled = false

  var softHeart1 = 0d
  var softHeart2 = 0d
  var triggerPull1 = false
  var triggerPull2 = false
  var pulltime = 0
  def sincePull(i: Int): Boolean = since(pulltime) > i.seconds

  var frames = 0L
  def renderFrame(): Unit = {
    frames += 1

    // Generate/Get heart signals
    if (!PulseSensor.init_) {
      println("Using fake pulse")
      PulseSensor.fake = true
      PulseSensor.init_ = true
    }
    val beat = PulseSensor.takeBeat
    val heart = if (beat) 1d else 0d
    softHeart1 = (heart + softHeart1)*0.8
    softHeart2 = (softHeart1 + softHeart2)*0.7

    osc1 = oscillator(phi = 0)
    osc2 = oscillator(phi = Pi/4)
    osc3 = oscillator(phi = Pi/2)
    osc4 = oscillator(phi = 3*Pi/4)

    if (fade1 < 1) fade1 += fadeSpeed1 else fade1 = 1
    if (fade12 < 1) fade12 += fadeSpeed12 else fade12 = 1
    if (fade2 < 1) fade2 += fadeSpeed2 else fade2 = 1
    if (fade22 < 1) fade22 += fadeSpeed22 else fade22 = 1
    if (fadeFlash < 1) fadeFlash += fadeSpeedFlash else fadeFlash = 1
    if (fadeSlow < 1) fadeSlow += fadeSpeedSlow else fadeSlow = 1

    val (mw, mh) = (200-osc1*100-osc3*30, 160-osc2*50-osc3*20)
    val (cx, cy) = (winWidth/2 - mw/2, winHeight/2 - mh/2)
    val ratio = mw/mh.toFloat

    if (shouldFreezeview) {
      freezeView += 0.0004 * renderTime
      if (freezeView > 1) freezeView = 1
    }

    // Get head rotation 2d, 3d
    val (rotx, roty) = (
      if (shouldFreezeview) {
        /*val desiredRotation = rotation0
        val rot = desiredRotation
        val ratio = 0.98f
        val rotDelta = (rot - prevRotation) * (1-ratio)
        rotation = rotation + rotDelta*/
        rotation = rotation toCenter 0.2f

        //prevRotation = (rot * (1-ratio)) + (prevRotation * ratio)

        //prevRotation = (rot * (1-ratio)) + (prevRotation * ratio)

        (-rotation.yaw*10, -rotation.pitch*10)
      } else {
        if (prevRotation == rotation0) {
          prevRotation = RiftTracker.poll

          (0f, 0f)
        } else {
          val rot = RiftTracker.poll
          val rotDelta = rot - prevRotation

          //println(rot)
          //if (framesf%2 == 0) {
          //  println(rot)
          //}
          rotation = rotation + rotDelta
          prevRotation = rot//prevRotation.avgW(rot, 0.9f)

          (-rotation.yaw*10, -rotation.pitch*10)
        }
      }
    )


    // ---
    if (prevRotation3d == rotation0) {
      prevRotation3d = RiftTracker.poll
    } else {
      val rot = RiftTracker.poll
      val rotDelta = rot - prevRotation3d

      rotation3d = rotation3d + rotDelta
      // probaby fail at 180..-180 mark
      val ratio = 0.92f
      prevRotation3d = (rot * (1-ratio)) + (prevRotation3d * ratio)
    }


    currentPhase match {
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Setup => ///////////////////////////////////////////////////////////////////////////////
        glClear1d(0)

        quad(Coord(winWidth/2-940/2,winHeight/2-550/2, 940,550), if (frames <= 10 && !startLiminoid) liminoidSplashLoading else liminoidSplash)

        //drawFrontCam()
        /*locally {
          val img = frontCamera.getTextureID
          val camScale = -1030
          val camhCorrect = 650
          val (camw, camh) = ((winHeight*(16d/9d)).toInt, winHeight+camhCorrect)
          val (camx, camy) = (winWidth/2-camw/2, -camhCorrect/2)
          quad(Coord(camx,camy, camw,camh)+camScale, img, flipy = flipFrontCamera, flipx = true)
        }*/

        //quad(Coord(winWidth/2-40/2,winHeight/2-650/2, 40,650), alpha = 1-testNum2*0.1)

        //startLiminoid = true
        val startingPhase = phases(settings("startingPhase"))
        //startingPhase = Mandalas
        //startingPhase = Schizoid
        //gotoPhase(Threads)

        // Triggers lazy load or preload of some resources
        val loadTime = Utils.time(
          frames match {
            case 1 =>
            case 2 => if (startingPhase <= Radiolarians || startingPhase == FlyingRock) radiolarian
            case 3 => if (startingPhase <= Radiolarians || startingPhase == FlyingRock) {
              _whiteRocks
              _rocks

              type P = Vec
              object Point {
                def convexHull(_points: Seq[P]): Seq[P] = {
                  if (_points.isEmpty) return _points
                  val points = _points.sortBy(_.x)
                  val upper = halfHull(points)
                  val lower = halfHull(points.reverse)
                  upper.remove(0)
                  lower.remove(0)
                  upper ++: lower
                }

                private def halfHull(points: Seq[P]) = {
                  val upper = new mutable.ListBuffer[P]()
                  for (p <- points) {
                    while (upper.size >= 2 && leftTurn(p, upper(0), upper(1))) {
                      upper.remove(0)
                    }
                    upper.prepend(p)
                  }
                  upper
                }

                private def leftTurn(p1: P, p2: P, p3: P) = {
                  val slope = (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
                  val collinear = math.abs(slope) <= 1e-9
                  val leftTurn = slope < 0
                  collinear || leftTurn
                }
              }

              val xxx = 40
              val pxpx = Vec(xxx, xxx, 0)
              val mxmx = Vec(-xxx, -xxx, 0)
              val pxmx = Vec(xxx, -xxx, 0)
              val mxpx = Vec(-xxx, xxx, 0)

              val hull = Point.convexHull(
                _whiteRocks.map(x => x.transform.pos) ++
                _whiteRocks.map(x => x.transform.pos + pxpx) ++
                _whiteRocks.map(x => x.transform.pos + mxmx) ++
                _whiteRocks.map(x => x.transform.pos + pxmx) ++
                _whiteRocks.map(x => x.transform.pos + mxpx)
              )
              val poly = new java.awt.Polygon()
              for (pt <- hull) poly.addPoint(pt.x.toInt, pt.y.toInt)

              filteredRocks = _whiteRocks ++ _rocks.filterNot { rock =>
                poly.contains(rock.transform.pos.x, rock.transform.pos.y)
              }
            }
            case 4 =>
            case 5 => if (startingPhase <= Mandalas) blackMandala.preload(300)
            case 6 => if (startingPhase <= Mandalas) blackHeartMandala
            case 7 => if (startingPhase <= Mandalas) blackHeartDustMandala
            case 8 => if (startingPhase <= Mandalas) whiteHeartMandala
            case 9 => if (startingPhase <= Mandalas) whiteMandala.preload(200)
            case 10 => if (startingPhase <= Schizoid) {
            }
            case _ => if (startLiminoid) gotoPhase(startingPhase) else sleep(20)
          })

        if (frames <= 11) println("Time" + frames + ": " + loadTime)

      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Limbo => ///////////////////////////////////////////////////////////////////////////////
        //Limbo phase, should never happen :)
        glClear1d(1)

        drawFrontCam()


      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Radiolarians => ////////////////////////////////////////////////////////////////////////
        initPhase {
          Sound.play("intro")
          for (i <- 1 to 10) frontCamera.getTextureIDWait()
        }

        /// Parts of the Radiolarians phase
        val izSteneStart     = 17.seconds//17.seconds
        val tresenjeStart    = 35.seconds
        val radioVectorStart = (60+25-38).seconds
        val radioOpenStart   = (60+43-38).seconds
        val endStart         = (120+8-38).seconds

        // Crawl from wall
        val izStene = since(phaseTimer) > izSteneStart
        val firstIzStene = !prevIzStene && izStene

        // Start shaking elements
        val tresenje = since(phaseTimer) > tresenjeStart
        val firstTresenje = !prevTresenje && tresenje

        // Pull radiolarian towards view
        val radioVector = since(phaseTimer) > radioVectorStart
        val firstRadioVector = !prevRadioVector && radioVector

        // Activate radiolarian shell open animation
        val radioOpen = since(phaseTimer) > radioOpenStart
        val firstRadioOpen = !prevRadioOpen && radioOpen

        if (firstRadioOpen) radiolarian.active = true
        val radioHalfOpen = radioOpen && radiolarian.cursor > radiolarian.frames.length/2

        // Start of radiolarians phase
        val end = since(phaseTimer) > endStart
        val firstEnd = !prevEnd && end

        if ((radiolarian.transform.pos distance vec0) < 1
        || (radiolarian.transform.pos.z < -1)) {
          gotoPhase(Mandalas)
        }

        setupLights()

        /// Rendering
        glClear1d(0)
        drawFrontCam()

        val posVec = vec0
          //if (testNum1 == 0 && testNum2 == 0 && testNum3 == 0) vec0 else
          //Vec(testNum1*5, testNum2*5, testNum3*5)

        radiolarian.transform.pos = radiolarian.transform.pos + posVec

        // Render Camera
        Render.cam.lookAt(Vec3(0, 0, 500))
        Render.cam.render()
        val rotationCalibrationp = 4 + (((testNum1-35+30)*10)-11-28*10+700)/100f
        val rotationCalibrationy = 4 + (((testNum2-35+30)*10)-11-13*10+700)/100f
        val rotationCalibrationr = 4 + (((testNum3-35+91+10)*10)-11*10+700)/100f

        Render.cam.rot =
          Vec3(
            -rotation3d.pitch/rotationCalibrationp,
            +rotation3d.yaw/rotationCalibrationy,
            +rotation3d.roll/rotationCalibrationr)

        //testNum1 = 0
        //testNum2 = 0
        //testNum3 = 0

        if (izStene) {
          // Draw invisible wall
          glCapabilities(GL_DEPTH_TEST, GL_BLEND) {
            glTheUsualBlendFunc()
            glColor4f(1, 1, 1, 0)
            render3D {
              glPrimitive(GL_QUADS) {
                glVertex3d(-2000, -2000, wallZ)// + osc1*10)
                glVertex3d(+2000, -2000, wallZ)// + osc2*10)
                glVertex3d(+2000, +2000, wallZ)// + osc3*10)
                glVertex3d(-2000, +2000, wallZ)// + osc4*10)
              }
            }
          }

          // Shake radiolarian
          if (firstTresenje) {
            fade1 = 0
          }
          def shake(m: Model): Transform = {
            val newPos =
              if (tresenje) {
                val osc = fade1 * abs(oscillator(phi = m.phi))
                m.transform.pos + Vec(
                  (TableRandom.nextGaussian/5)  * osc,
                  (TableRandom.nextGaussian/8)  * osc,
                  (TableRandom.nextGaussian/15) * osc)
              } else {
                m.transform.pos
              }

            m.transform.copy(pos = newPos)
          }

          // Draw radiolarian
          val oscDiv = 10d
          val radiolarianSize = Vec(1 + osc1/oscDiv, 1 + osc2/oscDiv, 1 + osc3/oscDiv)
          if (!pause) radiolarian.transform += radiolarian.transformVector ** renderTime
          if (firstRadioVector) {
            radiolarian.transformVector.pos = radiolarian.transformVector.pos.withZ(radioBasePosVecZ*1.75)
          }

          if (firstRadioOpen) {
            fade1 = 0
          }

          radiolarian.transform.size = {
            val scale = 1.7

            (if (radioOpen) {
              (radiolarianSize * (scale * (1 - fade1))) + vec((scale + 1/oscDiv) * fade1)
            } else {
              radiolarianSize * scale
            })
          }


          val shaked: Transform = if (radioOpen) radiolarian.transform else shake(radiolarian())
          radiolarian().render(transform = shaked) // duplication below

          // Make core go black after radiolarian opening
          if (radioHalfOpen) core.color -= 0.0002

          core.render(transform = shaked.copy(size = radiolarian.transform.size * radiolarian.coreTransform.pos.x))

          // Draw rocks
          glMatrix {
            for (rock <- filteredRocks) {
              if (!pause) rock.transform += rock.transformVector ** renderTime
              if (radioVector) {
                rock.transformVector.pos = rock.transformVector.pos.withZ(rock.transformVector.pos.z*0.85)
              }

              rock.render(transform = shake(rock))
            } ////////////////////////////////////////////// duplication
          }
        }

        prevIzStene = izStene
        prevTresenje = tresenje
        prevRadioVector = radioVector
        prevRadioOpen = radioOpen
        prevEnd = end

        //backBlendRender


      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Mandalas => ////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade1 = 0
          fade2 = 0
          Sound.play("mandalas")
        }

        val div = 1.75d
        val heartBeating = softHeart2*9
        val (w, h) =
            (640*0.85 + (if (blackMandala.active) -150 else heartBeating),
             800*0.85 + (if (blackMandala.active) -150 else heartBeating))
        val posx = winWidth/2-w/2d + rotx/div
        val posy = winHeight/2-h/2d + roty/div //don't forget the duplication below...

        if (blackMandala.cursor < 2000) {
          fadeSlow = 0
        }
        glClear1d(if (blackMandala.active) 0 else 1)
        if (blackMandala.active) {
          quad(Coord(posx,posy, w,h), blackMandala())
          if (fade2 < 1) quad(coord2000, alpha = 1 - fade2, color = grey0)

          fade1 = 0
        } else if (whiteMandala.active) {
          quad(Coord(posx,posy, w,h), whiteMandala())
        } else if (!blackMandala.active && !whiteMandala.active) {
          gotoPhase(Schizoid)
        }

        if (blackMandala.cursor > 5001-startFrameNum) {
          if (startDustHeart == -1) startDustHeart = now

          if (beat) {
            Sound.play(if (blackMandala.active) "heartbeep" else "heartbeat")
          }

          if (blackMandala.active) {
            if (since(startDustHeart) > 10.seconds) {
              val (ww, hh) = (w*0.8, h*0.8)
              val posx = winWidth/2-ww/2d + rotx/div
              val posy = winHeight/2-hh/2d + roty/div
              quad(Coord(posx,posy, ww,hh), blackHeartMandala, alpha = softHeart2*0.6)
            }

            quad(Coord(posx,posy, w,h), blackHeartDustMandala, alpha = softHeart1)
          }
        }

        if (whiteMandala.cursor > 1440) {
          whiteMandala.active = false
          quad(coord2000, alpha = fade1)
        } else if (whiteMandala.cursor > 1175) {
          quad(coord2000, alpha = fade1)
        } else if (whiteMandala.cursor > 1000) {
          fade1 = 0
        }

        //backBlendRender


      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Schizoid => ////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade1 = 0
          fade12 = 0
          fade2 = 1
          fade22 = 1
          diffStarted = false
          diffDone = false

          Texture.dumpCache()
          blackMandala = null
          whiteMandala = null

          backPixels = Vector.empty
          noisePixels = Vector.empty
          backPixelDrop = true

          back0Seq.preload(50)
          back1Seq.preload(100)
          back2Seq.preload(100)

          for (i <- 1 to 7) backCamera.getTextureIDWait
          Sound.play("introcingl")
          Sound.play("razpaddron")

          System.gc()
          System.runFinalization()
          System.gc()
          System.runFinalization()
        }

        glClear3d(0.175, 0.15, 0.15)
        if (fade12 < 0.9) {
          phaseTimer = now
        }

        val schEnd = 6
        val break1 = 15
        //val schEnd = 6-4
        //val break1 = 1
        val break2 = break1 + 8 + 5
        val break3 = break2 + 2 + 5
        if (schizoidPhase >= schEnd) {
          schizoidPhase = schEnd
          //phaseTimer = now
        }

        val rotlimitx = (-415, 415)
        val rotlimity = (-92, -62)
        val rotxl =
          if (rotx < rotlimitx._1) rotlimitx._1
          else if (rotx > rotlimitx._2) rotlimitx._2
          else rotx
        val rotyl =
          if (roty < rotlimity._1) rotlimity._1
          else if (roty > rotlimity._2) rotlimity._2
          else roty//*/

        val f = 1400d -600 + 7*20 + (-14/*+50*/)*20
        val (camresx, camresy) = (backCamera.width, backCamera.height)
        val camaspect = camresx/camresy.toDouble
        val (camw, camh) = (f*camaspect, f+testNum3+100) //(winHeight*4/3d, winHeight)
        val (camx, camy) = (rotxl*0.7-camw/7+  211, rotyl*0.7-camh/7 + 166)

        val back0 @ (back0Frame, back0Snap) = (back0Seq, back0Seq.snap)//(backCamera.getTextureID(), null)//(back0Seq(), back0Seq.snap)
        val backC @ (backCFrame, backCSnap) = (null, null)//if (noiseWall) back0 else (backCamera.getTextureID(), null)
        //val backC @ (backCFrame, backCSnap) = (back2Seq, back2Seq.snap)//if (noiseWall) back0 else (backCamera.getTextureID(), null)
        val back1 @ (back1Frame, back1Snap) = (back1Seq, back1Seq.snap)//backC//(back1Seq(), back1Seq.snap)
        val back2 @ (back2Frame, back2Snap) = (back2Seq, back2Seq.snap)//backC//(back2Seq(), back2Seq.snap)

        val _phasesSnap =
          Array(
            backCSnap, backCSnap,
            back0Snap, back1Snap,
            back0Snap, back2Snap,
            back0Snap, back1Snap,
            back0Snap, back2Snap,
            back0Snap, backCSnap)
        var _camSnap: Array[Int] = null
        def phasesSnap(i: Int): Array[Int] = {
          val snap = _phasesSnap(i)
          if (snap == null) {
             if (_camSnap == null) {
               _camSnap = backCamera.getSnap()
             }
             _camSnap
          } else {
            snap
          }
        }

        lazy val camTex = backCamera.getTextureID()

        val phasesSeq =
          Array(
            backCFrame, backCFrame,
            back0Frame, back1Frame,
            back0Frame, back2Frame,
            back0Frame, backCFrame,
            back0Frame, back1Frame,
            back0Frame, back2Frame)
        val phases =
          phasesSeq.map( p => if (p == null) camTex else p())

        quad(Coord(camx,camy, camw,camh), phases(schizoidPhase), flipx = false)
        if (fade22 < 1) {
          quad(Coord(camx,camy, camw,camh), phases(schizoidPhase-1), flipx = false, alpha = 1-fade22)
        }

        def resetStuff(): Unit = {
          phaseTimer = now
          backPixelDrop = true
          finished = false

          diffDone = false
          diffStarted = false
        }

        def sincen(i: Int) = since(phaseTimer) >= i.seconds
        def sinceWall(i: Int) = since(wallTimer) >= i.seconds

        if (sincen(break1) && backPixelDrop && !finished) {
          if (!diffStarted) thread {
            diffDone = false
            diffStarted = true
            //phasesSeq.foreach(x => if (x != null) x.rewind())
            bpAreDying = false
            backPixels = backCamera.getDiffBlob(phasesSnap(schizoidPhase+1), phasesSnap(schizoidPhase), settings("threshold").toInt, 1280, 720, pixelboxpoly)

            //backPixels = backPixels.sortBy(bp => abs(bp.x - bpCenterx)).dropRight((backPixels.size*0.1).toInt)
            //backPixels = backPixels.sortBy(bp => abs(bp.y - bpCentery)).dropRight((backPixels.size*0.1).toInt)
            //backPixels = backPixels.filter(bp => polygon.contains(bp.x, bp.y))

            val (bpcenterx, bpcentery) = {
              var x, y = 0d
              for (bp <- backPixels) {
                x += bp.x
                y += bp.y
              }

              (x/backPixels.size, y/backPixels.size)
            }

            for (bp <- backPixels) {
              bp.transformVector = Vec(
                  (bp.x - bpcenterx)/20d + TableRandom.nextGaussianUnsafe/0.5d,
                  (bp.y - bpcentery)/20d + TableRandom.nextGaussianUnsafe/0.5d,
                  0)
            }

            if (!noiseWall && schizoidPhase >= schEnd-1) {
              //Sound.stopAll()
              //Sound.play("ziddron")
              Sound.play("razpadheart1")
              Sound.play("razpad3")
              wallTimer = now
            } else {
              Sound.play("razpadheart1")
              Sound.play("razpad3")
            }
            middlemoment = false
            diffDone = true
          } else if (diffDone) {
            diffStarted = false
            diffDone = false
            backPixelDrop = false
            backPixelMerged = false
            backPixelMerge = true
            backPixels.foreach { bp =>
              bp.newColor = Color.BGR(phasesSnap(schizoidPhase+2)((bp.sx + bp.sy*1280).toInt))
              val esx = bp.sx
              val esy = bp.sy
              bp.sx += bp.transformVector.x*6d*(TableRandom.nextDouble*0.6+0.5) + TableRandom.nextGaussianUnsafe/0.6d
              if (bp.sx > 1280) bp.sx = esx
              bp.sy += bp.transformVector.y*6d*(TableRandom.nextDouble*0.6+0.5) + TableRandom.nextGaussianUnsafe/0.6d
              if (bp.sy > 720) bp.sy = esy

              try {
                //bp.newColor = Color.BGR(phasesSnap(schizoidPhase+2)((bp.sx + bp.sy*1280).toInt))
              } catch {
                case t: Throwable =>
              }
            }
            schizoidPhase += 1
          }
        }  else if (sincen(break2) && backPixelMerge) {
          backPixelMerge = false
          backPixelMerged = true
          middlemoment = true
          // somehow sneak pixles here
          /*backPixels.
          backPixels ++= Array.tabulate(1000) { i =>
            val bp = backPixels(nextInt(backPixels.size))
            new Pixel)
          }*/
        } else if (sincen(break1+2) && !noiseWall && schizoidPhase >= schEnd) {
          schizoidPhase += 1
          noiseWall = true
          //backPixels = Vector.empty
          //backPixels = backPixels.take(123456+1234);
          //println("dfwefwe")
          noiseFrames = 1
          backPixelMerged = true
          //backPixels.foreach { bp =>
            //if (TableRandom.nextDouble < 0.2) bp.isDying = true
            /*if (nextBoolean)
              bp.isDying = true
            else
              bp.isDead = true*/
          //}
        } else if (sincen(break3) && !noiseWall && backPixelMerged) {
          fade22 = 0.1
          phasesSeq.foreach(x => if (x != null) x.rewind())
          schizoidPhase += 1
          backPixelMerged = false
        } else if (sincen(break3) && !noiseWall && fade22 >= 0.95) {
          //Sound.play("razpadheart2")
          bpAreDying = true

          resetStuff()
        }
        if (!noiseWall) {
          wallTimer = now
        }

        /*if (schizoidPhase >= schEnd && !) {
          noiseWall = true
        }*/
        if (debugPoly) render2D {
          glMatrix {
            glTranslated(camx, camy, 0)
            glScaled(camw/camresx, camh/camresy, 1)
            glColor3d(1,1,1)
            glPrimitive(GL_LINE_LOOP) {
              for (p <- pts) {
                import p._
                glVertex2d(x, y)
              }
            }
            glPrimitive(GL_LINE_LOOP) {
              for (p <- pixelbox) {
                import p._
                glVertex2d(x, y)
              }
            }
          }
        }

        val threadNetworkfullyVisible = triggerPull1 || threadNetwork.fullyVisible

        //val npix = if (triggerPull1) 123456+nextInt(1234) else 100000
        if (noiseWall && !triggerPull1 && noisePixels.size < 111111+nextInt(1111)) {
        //if (noiseFrames > 0) {
          noiseFrames += 1

          val noiseLimit = 2133
          if (noiseFrames > noiseLimit) noiseFrames = noiseLimit
          val n = (noiseFrames*6+nextInt(noiseFrames*2))/2
          noisePixels ++= Array.fill(n)(
            makePix(
               bound.getX+TableRandom.nextDoubleUnsafe*bound.getWidth,
               bound.getY+TableRandom.nextDoubleUnsafe*bound.getHeight, 170/255d)
          )
          if (backPixels.nonEmpty) {
            noisePixels ++= Array.fill(n) {
              val bp = backPixels(nextInt(backPixels.size))
              bp.isDying = true
              makePix(bp.x, bp.y, 170)
            }
          }
        }

        val (bpcenterx, bpcentery) = (640-13+9 + testNum1, 360+19+7-5 + testNum2)
        val contain = frames%2 == 0
        if (backPixels.nonEmpty) {
          val noisePixOverLod = noisePixels.size > 50000
          val posRatio = if (triggerPull1) 0.85 else 0.982
          val posRatio1m = 1 - posRatio
          val colorRatio = posRatio
          val colorRatio1m = 1 - colorRatio
          if (!triggerPull1 || (triggerPull1 && frames%4 == 0)) {
            backPixels = backPixels.filterNot { bp =>
              val rand = TableRandom.nextDoubleUnsafe < 0.35
              if ((bp.isDying || (noisePixOverLod && rand && nextBoolean && nextBoolean)) && rand) {
                bp.isDead = true
              } else {
                if (schizoidPhase < schEnd && middlemoment || triggerPull1) {
                  bp.x = bp.x * posRatio + bp.sx*posRatio1m
                  bp.y = bp.y * posRatio + bp.sy*posRatio1m
                }
                bp.transformVector **= 0.97
                //if (!middlemoment) {
                  bp.transformVector ++= Vec.randomGaussianUnsafe(0.02)
                //}
                bp.x += bp.transformVector.x
                bp.y += bp.transformVector.y
                if (contain) {
                  bp.isContained = polygon.contains(bp.x, bp.y)
                }
                if (bp.isContained) {
                  bp.isFlipped = false
                } else {
                  if (noiseWall) {
                    bp.isDead = true
                  } else if (!bp.isFlipped) {
                    bp.transformVector.fliptwo(0.75)
                    bp.isFlipped = true
                  }
                }
                if (middlemoment && bp.color != bp.newColor) {
                  bp.color.r = bp.color.r * colorRatio + bp.newColor.r * colorRatio1m
                  bp.color.g = bp.color.g * colorRatio + bp.newColor.g * colorRatio1m
                  bp.color.b = bp.color.b * colorRatio + bp.newColor.b * colorRatio1m
                }
              }
              if (bpAreDying && rand && nextBoolean && nextBoolean && nextBoolean) {
                bp.isDying = true
              }

              bp.isDead
            }
          }


          glDisable(GL_BLEND)
          render2D {
            glMatrix {
              glTranslated(camx, camy, 0)
              glScaled(camw/camresx, camh/camresy, 1)
              //glScaled(camw/camw, camh/camh, 1)
              //glTranslated(camx*(1920/1280d)+testNum1, camy*(1080/720d)+testNum2, 0)
              glPrimitive(GL_QUADS) {
                for (bp <- backPixels) {
                  if (bp.isContained) {
                    bp.render()
                  }
                }
              }
            }
          }
        }

        tuptupcounted = tuptupcounter > 2

        if (noisePixels.nonEmpty) {
          val posRatio = if (spreadBall) 0.94 else if (triggerPull1) 0.97 else 0.99
          val posRatio1m = 1 - posRatio
          val colorRatio = posRatio
          val colorRatio1m = 1 - colorRatio
          if (!triggerPull1 || (triggerPull1 && frames%4 == 0)) {
            noisePixels = noisePixels.filter { bp =>
              val rand = TableRandom.nextDoubleUnsafe < 0.35
              if (schizoidPhase < schEnd || triggerPull1) {
                bp.x = bp.x * posRatio + bp.sx*posRatio1m
                bp.y = bp.y * posRatio + bp.sy*posRatio1m
              }
              bp.transformVector **= 0.95
              bp.transformVector ++= Vec.randomGaussianUnsafe(0.07)
              bp.x += bp.transformVector.x
              bp.y += bp.transformVector.y
              if (polygon.contains(bp.x, bp.y) || tuptupcounted) {
                if (threadNetworkfullyVisible) {
                  //val tup = (softHeart2)*200
                  /*if (tuptupcounter > 10) {
                    ballSize += 1
                  }*/
                  bp.sx = bpcenterx + TableRandom.nextGaussianUnsafe*(77+ballSize)
                  bp.sy = bpcentery + TableRandom.nextGaussianUnsafe*(77+ballSize)
                  if (spreadBall) {
                    ballSize = ballSize * 0.92
                  }
                }

                //if (tuptupcounter > 10) rand || nextBoolean || nextBoolean
                //else
                if (spreadBall) {
                  rand || nextBoolean || nextBoolean || nextBoolean
                } else {
                  true
                }
              } else {
                false
              }
            }
          }


          glDisable(GL_BLEND)
          render2D {
            glMatrix {
              glTranslated(camx, camy, 0)
              glScaled(camw/camresx, camh/camresy, 1)
              glPrimitive(GL_QUADS) {
                for (bp <- noisePixels) {
                  bp.render()
                }
              }
            }
          }
        }

        glColor3d(1,1,1)

        if (fade12 < 1) {
          //fade1 = 1
          glDisable(GL_TEXTURE_2D)
          quad(coord2000, alpha = 1-fade12)
        }


        if (tuptupcounted) {
          spreadBall = true
        }
        if (noisePixels.size < 21 && spreadBall) {
          spreadBallComplete = true
        }
        rockfly = spreadBallComplete

        val wallZ = 100-10
        // Draw invisible wall
        /*glCapabilities(GL_DEPTH_TEST, GL_BLEND) {
          glTheUsualBlendFunc
          glColor4f(1, 1, 1, 0)
          render3D {
            glPrimitive(GL_QUADS) {
              glVertex3d(-2000, -2000, wallZ)
              glVertex3d(+2000, -2000, wallZ)
              glVertex3d(+2000, +2000, wallZ)
              glVertex3d(-2000, +2000, wallZ)
            }
          }
        }*/

        val wallTiem = 10
        //val wallTiem = 5
        if (schizoidPhase >= schEnd && sinceWall(wallTiem) || testNum5 != 0) {
          if (!networkStarted) {
            networkStarted = true
            Sound.play("networkdron")
          }
          implicit val rpd = new RenderProcessData(
              beat, triggerPull2,
              bpcenterx-9, bpcentery-7,
              if (sinceWall(wallTiem+3) || testNum6 != 0) 0.965 else 1)
          implicit val rrd = new RenderRenderData(
              camx+networkOffsetx.toInt, camy+networkOffsety.toInt,
              camw, camh,
              0/*if (tuptupcounted) 1.1 else if (tuptupenabled) softHeart2 else 0*/)
          threadNetwork.process
          threadNetwork.render
        }

        if (threadNetworkfullyVisible || testNum5 != 0) {
          val xxxx = 5-29+26+ (4-3-1 + testNum5)/10f
          val yyyy = -8+3-2 + (-6+10+44-7+3+23 + testNum6)/10f
          val hhhh = 15/1000d
          val rrrr = 0.1f + 80/1000f
          val ssss = 199/1000d + -0.3
          val vvx1 = (testNum5*0 -2)/1000f
          val vvy1 = (testNum6*0)/1000f
          val vvx2 = (testNum5*0 + 2)/1000f
          val vvy2 = (testNum6*0 + 7)/1000f
          core.transform.size = Vec(csize, csize, csize)
          core.color = grey(0.80+(-23-8+testNum4)/100f)
          if (!triggerPull1) {
            triggerPull1 = true
            pulltime = now

            // Init last 3d rock
            core.transform.pos = Vec(xxxx, yyyy, 100)
            //core.transformVector.pos = Vec(0, hhhh, ssss)
            core.transformVector.pos = Vec(vvx2, vvy2, ssss)
            core.transformVector.rot = Vec(rrrr, rrrr, rrrr)
          }
          if (!triggerPull2 && sincePull(8)) {
            triggerPull2 = true
            Sound.play("odcep")
          }
          if (threadNetwork.fullyOver) {
            if (!shouldFreezeview) {
              Sound.play("over")
              shouldFreezeview = true
            }
            if (viewFrozen) {
              tuptupenabled = true
              if (tuptupenabled && beat) {
                //Sound.play("heartbeat")
                tuptupcounter += 1
                println("tuptup: "+tuptupcounter)
              }

              if (!tupRockDisabled && !tupRockEnabled && (core.transform.pos.z < 70)) {
                tupRockEnabled = true
              }
              if (!tupRockDisabled && tupRockEnabled && beat) {
                Sound.play("heartbeat")
                tupRock += 1
                println("tupRock: " + tupRock)
              }
              if (tupRockDisabled && tupRockEnabled) {
                tupRockEnabled = false
              }

              if (tuptupcounted && spreadBallComplete) {
                csize = if (tupRockDisabled) csize*0.91 + 0.6*0.09 else if (tupRockEnabled) 0.5+0.1*softHeart2 else csize*0.98 + 0.5*0.02
                if (!rockOut) {
                  rockOut = true
                  //Sound.play("rockout")
                }
                backPixels = Vector.empty
                Render.cam.pos = Vec3(0, 0, 0)
                Render.cam.lookAt(Vec3(xxxx, yyyy, 500))
                Render.cam.render()
                // view is frozen
                Render.cam.rot = Vec3(0,0,0)

                //core.transform.pos += Vec(testNum6,testNum3,0)
                testSum = testNum5+testNum6
                if (testSum != exTestSum) {
                  core.transform.pos = Vec(xxxx, yyyy, 100)
                  //core.transformVector.pos = Vec(0, hhhh, ssss)
                  core.transformVector.pos = Vec(vvx2, vvy2, ssss)
                  core.transformVector.rot = Vec(rrrr, rrrr, rrrr)
                  core.color = grey(0.85-testNum5/100f)
                  rockOut = true
                }
                exTestSum = testSum
                if (tupRockEnabled) {
                  val t = Models.MutableTransform(
                      pos = vec0,
                      rot = vec0,
                      size = core.transformVector.size)
                  core.transform += t ** renderTime
                } else if (!tupRockDisabled) {
                  val t = Models.MutableTransform(
                      pos = Vec(vvx1, vvy1, core.transformVector.pos.z),
                      rot = vec0,
                      size = core.transformVector.size)
                  core.transform += t ** renderTime
                } else {
                  core.transform += core.transformVector ** renderTime
                }

                core.render(transform = core.transform)
              }
            }
          }
        } else {
          pulltime = now
        }

      case _ =>
    }
  }

  var flash = 0
  var sinceDown = now
  var isShutDown = false


  def processInput(): Unit = {
    import Keyboard._
    import Mouse._

    //if (isKeyDown(KEY_X)) Sound.play("jump")
    if (isKeyDown(KEY_1)) testNum1 -= 1
    if (isKeyDown(KEY_2)) testNum1 += 1
    if (isKeyDown(KEY_3)) testNum2 -= 1
    if (isKeyDown(KEY_4)) testNum2 += 1
    if (isKeyDown(KEY_5)) testNum3 -= 1
    if (isKeyDown(KEY_6)) testNum3 += 1
    if (isKeyDown(KEY_7)) testNum4 -= 1
    if (isKeyDown(KEY_8)) testNum4 += 1
    if (isKeyDown(KEY_H)) testNum5 -= 1
    if (isKeyDown(KEY_J)) testNum5 += 1
    if (isKeyDown(KEY_K)) testNum6 -= 1
    if (isKeyDown(KEY_L)) testNum6 += 1

    if (isKeyDown(KEY_F)) {
      shouldFreezeview = true
    }
    if (isKeyDown(KEY_G)) {
      shouldFreezeview = false
    }

    if (!startLiminoid && (isKeyDown(KEY_RETURN) || isButtonDown(0))) {
      println("Liminoid started!")
      startLiminoid = true
      return
    }

    if (isButtonDown(0) && isButtonDown(1)) {
      if (!isShutDown) {
        isShutDown = true
        sinceDown = now
      } else {
        if (since(sinceDown) > 4.seconds) {
          isMainLoopRunning = false
        }
      }

    }

    if (isKeyDown(KEY_Q)) rotation = rotation + Rotation(+1, 0,  0)
    if (isKeyDown(KEY_E)) rotation = rotation + Rotation(-1, 0,  0)
    if (isKeyDown(KEY_S)) rotation = rotation + Rotation(0, +1,  0)
    if (isKeyDown(KEY_W)) rotation = rotation + Rotation(0, -1,  0)
    if (isKeyDown(KEY_D)) rotation = rotation + Rotation(0,  0, +1)
    if (isKeyDown(KEY_A)) rotation = rotation + Rotation(0,  0, -1)
    if (isKeyDown(KEY_0) || isButtonDown(1)) {
      rotation = rotation0
      rotation3d = rotation0
      Render.cam.pos = Vec3(0, 0, 0)
      println("rotation reset")
    }
    if (isKeyDown(KEY_UP))    Render.cam.pos.z += 1
    if (isKeyDown(KEY_DOWN))  Render.cam.pos.z -= 1
    if (isKeyDown(KEY_LEFT))  Render.cam.pos.y -= 1
    if (isKeyDown(KEY_RIGHT)) Render.cam.pos.y += 1

    if (isKeyDown(KEY_B)) {
      for (i <- 1 to 1000) {
        makePix(winWidth/3, winHeight/3, 1)
      }
    }


    if (isKeyDown(KEY_NEXT)) nextPhase()
    if (isKeyDown(KEY_PRIOR)) previousPhase()
    if (isKeyDown(KEY_HOME)) gotoPhase(Radiolarians)
    if (isKeyDown(KEY_END)) gotoPhase(PhaseTerminator)
    //if (isKeyDown(KEY_P)) { pause = !pause; sleep(200) }
    if (isKeyDown(KEY_P)) { backPixels = Vector.empty; noisePixels = Vector.empty }

    if (isKeyDown(KEY_X)) {
      // Takes about 5 frames to set exposure, let's wait...
      for (i <- 1 to 10) backCamera.getTextureIDWait
      //backCamera.saveImage(dataFolder + "img/Image.png")
      backPixels = Vector.empty
      phaseTimer = now
      back0Seq.rewind()
      back1Seq.rewind()
      backPixelDrop = true
      backPixelMerge = false
      backPixelMerged = true
      finished = false
      diffStarted = false
      diffDone = false
      schizoidPhase = 1
      Sound.play("razpad")
    }
    if (isKeyDown(KEY_Z)) {
      println("Making new Back...")
      Sound.play("razpadheart1")
      // Takes about 5 frames to set exposure, let's wait 20...
      for (i <- 1 to 20) backCamera.getTextureIDWait
      val t = 22.seconds
      new java.io.File(settings("backN")).mkdir()
      //for (i <- 1 to n) {
      val time = now
      var i = 1
      while (since(time) < t) {
        backCamera.saveImage(settings("backN") + i + ".png")
        i += 1
      }
      backPixels = Vector.empty
      phaseTimer = now
      //backCamSnap = Texture.getImage(dataFolder + "seq/NewBackSpace/1.png")
      back0Seq.rewind()
      back1Seq.rewind()
      backPixelDrop = true
      backPixelMerge = false
      backPixelMerged = true
      finished = false
      diffStarted = false
      diffDone = false
      schizoidPhase = 1
      Sound.play("razpadheart1")
      println("Back done...")
    }
    if (isKeyDown(KEY_C)) {
      renderMode = if (renderMode == Mono) Stereo else Mono
      sleep(200)
    }
    if (isKeyDown(KEY_M)) {
      Sound.mute()
    }

    if (Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
      isMainLoopRunning = false
    }
  }

  def makePix(sx: Double, sy: Double, max: Double): NoisePixel = (
    new NoisePixel(sx, sy,
        Vec(TableRandom.nextGaussianUnsafe*2, TableRandom.nextGaussianUnsafe*2, 0),
        color = TableRandom.nextDoubleUnsafe*max)
  )

}
