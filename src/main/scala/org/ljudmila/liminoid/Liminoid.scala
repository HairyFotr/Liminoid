package org.ljudmila.liminoid

import org.lwjgl.opengl.{ Display, PixelFormat, DisplayMode, Util }
import org.lwjgl.input.{ Keyboard, Mouse }
import collection.mutable
import collection.parallel.mutable.ParArray
import java.nio._
import scala.actors.Futures._
import scala.util.Random._
import math._
import Utils._
import org.lwjgl.opengl.{ GL11, GL12, GL13, GL14 }
import org.lwjgl.opengl.GL11._
import hardware.{ RiftTracker, PulseSensor }
import scala.annotation.switch
import Thread.sleep

import Render.{ render3D, render2D }
import GLAddons._

import Model.{ Transform, OBJModel, quad, Coord, Point }
import Model.{ Color, grey, grey0, grey1 }
import Model.{ Vec, vec, vecx, vecy, vecz, vec0, vec05, vec1, vec2, vec3, vec4, vec5, vec90x }
import Model.{ Rotation, rotation0 }
import Model.{ Thread, ThreadNetwork, ThreadNode, Line, RenderProcessData }

final object Liminoid {
  val project = "Liminoid"
  var startLiminoid = false

  var renderMode: RenderMode = Stereo

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
    Sound.init()

    mainLoop()
    
    // Cleanup
    Sound.stopAll()
    RiftTracker.destroy()
    PulseSensor.close()
    Display.destroy()
  }

  var (winWidth, winHeight) = (1920, 1080)
  val (forceWidth, forceHeight) = (1920, 1080)
  def initDisplay(): Unit = {
    Display.setTitle(project)

    val bestMode =
      Display
        .getAvailableDisplayModes
        .filter(mode => mode.isFullscreenCapable && mode.getWidth == forceWidth && mode.getHeight == forceHeight)
        .maxBy(_.getBitsPerPixel)
    
    Display.setDisplayModeAndFullscreen(bestMode)
    Display.setVSyncEnabled(true)
    Display.setFullscreen(true)
    //winWidth = bestMode.getWidth
    //winHeight = bestMode.getHeight
    
    println("Display: "+bestMode.getWidth+"x"+bestMode.getHeight+"@"+bestMode.getFrequency+"Hz, "+bestMode.getBitsPerPixel+"bit")
    
    Display.create()
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
    val FPSseconds = 1
    var FPStimer = currentTime
    frameTime = currentTime

    isMainLoopRunning = true
    while(isMainLoopRunning) {
      processInput() // process keyboard input
      resetView()    // clear view and reset transformations

      renderMode match {
        case Mono =>
          renderFrame()    // draw stuff
          Display.update() // update window contents and process input messages
        case Stereo =>
          shader.beginOffScreenRenderPass()
          renderFrame()    // draw stuff
          shader.endOffScreenRenderPass()
          shader.renderToScreen()
          Display.update() // update window contents and process input messages
      }
      
      frameCounter += 1

      if(currentTime-FPStimer > second*FPSseconds) {
        val FPS = frameCounter/FPSseconds.toFloat

        println("FPS: "+FPS)
        println("sinceStart: "+since(phaseTimer))
        if(testNum1 != 0) println("testNum1: "+testNum1)
        if(testNum2 != 0) println("testNum2: "+testNum2)
        if(testNum3 != 0) println("testNum3: "+testNum3)
        if(phase == Radiolarians) println("radiolarian: "+radiolarian.transform)
        println("rotation:"+rotation)

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
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST)
    glEnable(GL_POINT_SMOOTH)
    glEnable(GL_LINE_SMOOTH)
    glEnable(GL_POLYGON_SMOOTH)
    
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

    glLight(GL_LIGHT0, GL_AMBIENT, floatBuffer(0.65f, 0.65f, 0.65f, 0.0f))
    glLight(GL_LIGHT0, GL_DIFFUSE, floatBuffer(0.7f, 0.7f, 0.7f, 0.0f))
    glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 20f)
    glLight(GL_LIGHT0, GL_POSITION, floatBuffer(0f, 0f, 10f, 0f))
    glEnable(GL_COLOR_MATERIAL)
    glMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, floatBuffer(0.9f, 0.9f, 0.9f, 0f))
    glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)
    
    glViewport(0,0, winWidth,winHeight) // mapping from normalized to window coordinates
  }
  
  /**
  * Resets the view of current frame
  */
  def resetView(): Unit = {
    // clear color and depth buffer
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
  }
  
  /**
  * Renders current frame
  */

  lazy val liminoidTitle = Texture("img/liminoid.png")
  lazy val liminoidTitleLoading = Texture("img/liminoidLoading.png")
  
  /// Liminoid phases ///
  val Setup = 0
  val Radiolarians = 1
  val Mandalas = 2
  val CircleSpace = 3
  val BackSpace = 4
  val PhaseTerminator = BackSpace // Last phase
  var phaseTimer = now // Tracks time from the beginning of phase

  var phase = Setup // Current phase
  var phaseChanged = true
  
  def gotoPhase(i: Int): Unit = {
    phase = i
    phaseChanged = true
  }
  def nextPhase(): Unit = {
    if(phaseChanged || phase >= PhaseTerminator) return
    phase += 1
    phaseChanged = true
  }
  def previousPhase(): Unit = {
    if(phaseChanged || phase <= Setup) return
    phase -= 1
    phaseChanged = true
  }
  def initPhase(initFunc: => Unit): Unit = 
    if(phaseChanged) {
      phaseTimer = now
      phaseChanged = false
      initFunc
      System.gc() // best place to do it...
    }

  var osc1 = 0d
  var osc2 = 0d
  var osc3 = 0d
  var osc4 = 0d
  
  // Cameras
  //val cams = Array(hardware.Camera(camId = 0, 1920, 1080), hardware.Camera(camId = 1, 1920, 1080), hardware.Camera(camId = 2, 1280, 720), hardware.Camera(camId = 3, 1280, 720))
  //val stereoCameras = cams.takeRight(2).reverse
  
  //val backCamera = hardware.Camera(camId = 1, 1920, 1080) //cams(0)
  //val frontCamera = hardware.Camera(camId = 0, 1280, 720) //960?
  //val flipFrontCamera = true
  
  //val backCamera = hardware.Camera(camId = 0, 1280, 960) //cams(0)
  //val frontCamera = hardware.Camera(camId = 1, 1920, 1080)
  
  val frontCamera = hardware.Camera(camId = 0, 640, 480)
  val backCamera = hardware.Camera(camId = 1, 640, 480)
  
  val flipFrontCamera = true//false

  val eyeCorrection = -64 // Eye shift for 2D rift view
  var testNum1, testNum2, testNum3 = 0 // Numbers for testing

  val winCoord = Coord(0,0, winWidth,winHeight)
  val coord2000 = Coord(0,0, 2000,2000)
  /// Radiolarians phase objects ///
  val whiteish = grey(0.9)
  val blackish = grey(0.1)

  val wallZ = 600                        // z position of wall
  var radioBasePosVec = Vec(0, 0, -0.11) // basic z movement vector
  val startPos = vecz(wallZ+15)          // starting point for radiolarians
  def basicRot(): Vec = Vec.randomGaussian(1d/3d)
  
  // The rock inside radiolarians
  lazy val core = OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(color = Color(0.2, 0.2, 0.2))

  // The radiolarian that opens up
  lazy val radiolarian =
    OBJSequence(
      path = "obj/OBJ_the_radiolarian_normale",
      active = false,
      stopAtEnd = true,
      delay = 80,
      color = whiteish,
      coreTransform = Transform(pos = vec3),
      transform = Transform(pos = startPos + vecz(-5)),
      transformVector = Transform(pos = radioBasePosVec, rot = vec0))
    
  // The other radiolaria
  lazy val quasiradiolarians =
    Array[Model.Model](
      /*OBJModel("obj/Plascki_iz_stene/Plascek_normale_VII.obj").toModel( // holes
        transform = Transform(pos = startPos + Vec(-45, 21, 2), size = vec(0.6)),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = whiteish),
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VII.obj").toModel( // holes
        transform = Transform(pos = startPos + Vec(25, -19, 9), size = vec(0.7)),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = whiteish),
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_V.obj").toModel( // Same as radiolarian
        transform = Transform(pos = startPos + Vec(48, -33, 5), size = vec(0.9)),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec3),
        color = whiteish), //majhen
      //OBJModel("obj/Ogromni_modeli/Plascek_normale_IV.obj").toModel( // Same ??
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_V.obj").toModel(
        transform = Transform(pos = startPos + Vec(30, 25, 5), size = vec(0.9)),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec3),
        color = whiteish), //majhen
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VI.obj").toModel( // Edgy one
        transform = Transform(pos = startPos + Vec(-53, -18, 9), size = vec(0.7)),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = whiteish),
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VI.obj").toModel( // Edgy one
        transform = Transform(pos = startPos + Vec(-15, -24, 8), size = vec(0.6)),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = whiteish) //,*/
      /*OBJModel("obj/Plascki_iz_stene/Plascek_normale_VII.obj").toModel( // Big holes
        transform = Transform(pos = startPos + Vec(35, 15, 8), size = vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = whiteish)*///,
      /*OBJModel("obj/Plascki_iz_stene/Plascek_normale_IX_mali.obj").toModel( // High-poly version: obj/Ogromni_modeli/Plascek_normale_IX_velik.obj
        transform = Transform(pos = startPos + Vec(-15, 37, 63), size = vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec3),
        color = whiteish)*///, //test it
      /*OBJModel("obj/Plascki_iz_stene/Plascek_normale_IX_mali.obj").toModel( // The thick one
        transform = Transform(pos = startPos + Vec(-5, -14, 82), size = vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec3),
        color = whiteish)*/)

  // Some rocks just floating around
  lazy val rocks = 
    Array(
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
        transform = Transform(pos = startPos + Vec(-25, 22, 12), size = vec3),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = whiteish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel(
        transform = Transform(pos = startPos + Vec(-40, -4, 4), size = vec4),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = blackish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
        transform = Transform(pos = startPos + Vec(-67, 18, 1), size = vec3),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = whiteish), ///////////////////////////////////////////////////////////////////duplication
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel(
        transform = Transform(pos = startPos + Vec(-75, -27, 13), size = vec4),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = blackish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
        transform = Transform(pos = startPos + Vec(-35, -35, 17), size = vec3),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = whiteish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel( // I has a cube and too many vertices
        transform = Transform(pos = startPos + Vec(35, 12, 11), size = vec2),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = whiteish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
        transform = Transform(pos = startPos + Vec(48, 5, 8), size = vec(2.6)),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = blackish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
        transform = Transform(pos = startPos + Vec(72, -15, 12), size = vec3),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = whiteish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
        transform = Transform(pos = startPos + Vec(65, -45, 15), size = vec2),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = whiteish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
        transform = Transform(pos = startPos + Vec(4, 35, 12), size = vec3),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        color = blackish),
      // ex radiolaria
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel( // holes
        transform = Transform(pos = startPos + Vec(-45, 21, 2), size = vec2),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = whiteish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel( // holes
        transform = Transform(pos = startPos + Vec(25, -19, 9), size = vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = blackish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel( // Same as radiolarian
        transform = Transform(pos = startPos + Vec(48, -33, 5), size = vec3),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec3),
        color = whiteish), //majhen
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel(
        transform = Transform(pos = startPos + Vec(30, 25, 5), size = vec3),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec3),
        color = whiteish), //majhen
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel( // Edgy one
        transform = Transform(pos = startPos + Vec(-53, -18, 9), size = vec2),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = blackish),
      OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel( // Edgy one
        transform = Transform(pos = startPos + Vec(-15, -24, 8), size = vec2),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = vec5),
        color = whiteish))

  // rocks that fly with the radiolarian
  lazy val guardRocks = Array(
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(-29, -13, 0), size = vec(2.2)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = whiteish),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(7, -32, 0), size = vec3),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = whiteish))

  /// Mandalas phase objects ///
  lazy val blackMandala = TexSequence("seq/optipng_Sekvenca_mandala_crno_ozadje", delay = 1000/24d, stopAtEnd = true, selfDestruct = true)
  lazy val whiteMandala = TexSequence("seq/optipng_Sekvenca_mandala_belo_ozadje", delay = (1000/24d)*0.8, stopAtEnd = true, selfDestruct = true)
  var whiteFlashTimer = -1
  var startHeart = -1
  var startDustHeart = -1
  var lastHeartpos = 0d
  lazy val blackHeartMandala     = Texture("seq/Srcni_utrip_CO/Srcni_utrip_CO_01290.png")
  lazy val blackHeartDustMandala = Texture("seq/Srcni_utrip_CO_II/Srcni_utrip_CO_II_01287.png")
  lazy val whiteHeartMandala     = Texture("seq/Srcni_utrip_BO/Srcni_utrip_BO_05848.png")
  var zoom = 0d

  /// CircleSpace phase objects ///
  lazy val sphereTex = Texture("img/sphere.png")
  lazy val sphereMandala =
    TexSequence("seq/optipng_Sfera_plasc_I",
    delay = 1000/10d,
    stopAtEnd = false,
    bounce = true)
    
  def newStar(): Model.Model = OBJModel("obj/UV_sfera/UV_sfera_I.obj").toModel(
    transform = Transform(rot = Vec.random, size = vec05 + (Vec.random/3)),
    transformVector = Transform(rot = Vec.random),
    color = Color(0.2, 0.2, 0.2),
    phi = nextDouble*math.Pi*2, theta = nextDouble*math.Pi*2)
  var stars = Vector.empty[Model.Model]

  var displayListSphere = -1

  lazy val magnets =
    Vector.fill(15)(
      OBJModel("obj/UV_sfera/UV_sfera_I.obj").toModel(
        transform = Transform(rot = Vec.random, size = vec2),
        transformVector = Transform(rot = Vec.random),
        color = grey(0.3),
        phi = nextDouble*math.Pi*2, theta = nextDouble*math.Pi*2))

  // BackSpace phase objects
  var wallTex = -1
  var backCamSnapSeq = TexSequence("seq/BackSpace/", delay = 1000/15d, stopAtEnd = false, bounce = true)
  var backCamSnap = Texture.getImage("seq/BackSpace/1.png")
  var backCamSnapTex = backCamera.getTextureIDWait
  var backPixels = Vector.empty[Model.Pixel]
  var backpixelBuffer = Array[Array[Boolean]]()
  var backPixelDrop = false
  var backPixelMerge = false
  var backPixelMerged = true
  var finished = false
  var diffStarted, diffDone = false
  lazy val liminoidBeingTex = Texture("img/liminoidno_bitje_I.png")
  
  //TODO: load from file
  val initNodes = 
    Vector(
      Point(100, -200),
      Point(-100, 200),
      Point(1400, 700)
    )
  val nodes = 
    Vector(
      Point(400, 700),
      Point(600, 500)
    )
  
  val lines = 
    Vector(
      Line(initNodes(0), nodes(0)),
      Line(initNodes(1), nodes(0)),
      Line(initNodes(2), nodes(1)))
  
  val threadMap = lines.map{ line => line -> Thread.generateMultiThread(3)(line) }.toMap
  
  lazy val threadNodes = 
    nodes.map{ node => 
      ThreadNode(
        node,
        ins = lines.find{ _.p2 eq node }.map { line =>
          threadMap(line)
        }.getOrElse(Seq.empty),
        outs = lines.find{ _.p1 eq node }.map { line =>
          threadMap(line)
        }.getOrElse(Seq.empty),
        liminoidBeingTex)
    
  }
  
  lazy val threadNetwork = ThreadNetwork(threadNodes);

  // variable that goes to 1 by little each frame
  var (fade1, fadeSpeed1) = (1d, 0.002)
  var (fade2, fadeSpeed2) = (1d, 0.04)

  // have a small bump for movement
  val shakeBump = 3
  val shakeBumpN = 50

  // Oculus rift head tracking
  var prevRotation, rotation = rotation0
  
  var prevIzStene = false
  var prevTresenje = false
  var prevRadioVector = false
  var prevRadioOpen = false
  var prevEnd = false

  def glClear(c: Double): Unit = {
    GL11.glClearColor(c.toFloat, c.toFloat, c.toFloat, 1)
    GL11.glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
  }
  def drawFrontCam(): Unit = {
    val img = frontCamera.getTextureID
    val camScale = -1030
    val camhCorrect = 650
    val (camw, camh) = ((winHeight*(16d/9d)).toInt, winHeight+camhCorrect)
    val (camx, camy) = (winWidth/2-camw/2, -camhCorrect/2)
    quad(Coord(camx,camy, camw,camh)+camScale, img, flipy = flipFrontCamera, flipx = true)
  }

  val backBlend = (GL_ONE, GL_ONE)
  val backBlendColor = Color(0, 0.08, 0.18)
  def backBlendRender(): Unit = {
    GL14.glBlendEquation(GL14.GL_FUNC_REVERSE_SUBTRACT)
    val alpha = if(phase == Radiolarians) 0.07 else 0.1
    quad(winCoord, color = backBlendColor, alpha = alpha, blend = backBlend)
    GL14.glBlendEquation(GL14.GL_FUNC_ADD)
  }

  var softHeart = 0d
  var softHeart2 = 0d

  var frames = 0L
  def renderFrame(): Unit = {
    frames += 1

    // Generate/Get oscilators, and heart signals
    if(!PulseSensor.init_) {
      println("Using fake pulse")
      PulseSensor.fake = true
      PulseSensor.init_ = true
    }
    var beat = PulseSensor.takeBeat
    val heart = if(beat) 1d else 0d
    softHeart = (heart + softHeart)*0.8
    softHeart2 = (softHeart + softHeart2)*0.7

    // http://www.dspguru.com/dsp/howtos/how-to-create-oscillators-in-software
    // f = desired oscillator frequency
    // w = 2 pi f / Fs
    // phi = initial phase
    // y = ouput array containing the oscillator signal
    // def oscillator(i: Double) = sin((2*Pi*f)*i + phi)
    def oscillator(i: Double = Utils.now*0.002, phi: Double): Double = sin(i + phi)

    osc1 = oscillator(phi = 0*Pi/4)
    osc2 = oscillator(phi = 1*Pi/4)
    osc3 = oscillator(phi = 2*Pi/4)
    osc4 = oscillator(phi = 3*Pi/4)

    val (mw, mh) = (200-osc1*100-osc3*30, 160-osc2*50-osc3*20)
    val (cx, cy) = (winWidth/2 - mw/2, winHeight/2 - mh/2)
    val ratio = mw/mh.toFloat

    // Get head rotation 2d, 3d
    val (rotx, roty) = (
      if(prevRotation == rotation0) {
        prevRotation = RiftTracker.poll
        
        (0f, 0f)
      } else {
        val rot = RiftTracker.poll
        val rotDelta = rot - prevRotation
        prevRotation = rot*0.1f + prevRotation*0.9f
        rotation = rotation + rotDelta
        
        (-rotation.yaw, -rotation.pitch)
      })

    if(fade1 < 1) fade1 += fadeSpeed1 else fade1 = 1
    if(fade2 < 1) fade2 += fadeSpeed2 else fade2 = 1

    phase match {
      
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Setup => ///////////////////////////////////////////////////////////////////////////////
        glClear(0)

        quad(Coord(winWidth/2-940/2,winHeight/2-550/2, 940,550), if(frames <= 10 && !startLiminoid) liminoidTitleLoading else liminoidTitle)

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
        var startingPhase = Radiolarians
        //startingPhase = Mandalas
        //startingPhase = BackSpace

        // Triggers lazy load or preload of some resources
        println("Time" + frames + ": " + Utils.time(
          frames match {
            case 1 =>
            case 2 => radiolarian
            case 3 => quasiradiolarians
            case 4 => rocks 
            case 5 => guardRocks
            case 6 => blackMandala.preload(200)
            case 7 => blackHeartMandala
            case 8 => blackHeartDustMandala
            case 9 => whiteHeartMandala
            case _ => if(startLiminoid) gotoPhase(startingPhase) else sleep(50)
          }))
        
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case -1 => //////////////////////////////////////////////////////////////////////////////////
        //Limbo phase, should never happen :)
        glClear(1)

        drawFrontCam()

      
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Radiolarians => ////////////////////////////////////////////////////////////////////////
        initPhase {
          Sound.play("intro")
          radiolarian.transform.rot = vec90x
        }

        /// Parts of the Radiolarians phase
        val izSteneEnding     = 17.seconds
        val tresenjeEnding    = 35.seconds
        val radioVectorEnding = (60+25).seconds
        val radioOpenEnding   = (60+43).seconds
        val endEnding         = (120+8).seconds

        // Crawl from wall
        var izStene = since(phaseTimer) > izSteneEnding
        val firstIzStene = (!prevIzStene && izStene)

        // Start shaking elements
        val tresenje = since(phaseTimer) > tresenjeEnding
        val firstTresenje = (!prevTresenje && tresenje)

        // Pull radiolarian towards view
        val radioVector = since(phaseTimer) > radioVectorEnding
        val firstRadioVector = (!prevRadioVector && radioVector)

        // Activate radiolarian shell open animation
        val radioOpen = since(phaseTimer) > radioOpenEnding
        if(radioOpen) radiolarian.active = true

        val firstRadioOpen = (!prevRadioOpen && radioOpen)

        // Ending of radiolarians phase
        val end = since(phaseTimer) > endEnding
        val firstEnd = (!prevEnd && end)

        if((radiolarian.transform.pos distance vec0) < 1 
        || (radiolarian.transform.pos.z < -1)) gotoPhase(Mandalas)


        /// Rendering
        glClear(0)
        drawFrontCam()

        val posVec =
          if(testNum1 == 0 && testNum2 == 0 && testNum3 == 0) vec0 else
          Vec(testNum1*5, testNum2*5, testNum3*5)
          
        radiolarian.transform.pos = radiolarian.transform.pos + posVec
        testNum1 = 0
        testNum2 = 0
        testNum3 = 0

        // Render Camera
        Render.cam.lookAt(Vec3(0, 0, 500))
        Render.cam.render
        val rotationCalibration = 4 + (testNum1+700)/100f
        Render.cam.rot = 
          Vec3(
            -rotation.pitch/rotationCalibration, 
            +rotation.yaw/rotationCalibration,
            +rotation.roll/rotationCalibration)

        if(izStene) {
          // Draw invisible wobbly wall
          glCapability(GL_DEPTH_TEST, GL_BLEND) {
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
            glColor4f(1, 1, 1, 0)
            render3D {
              glPrimitive(GL_QUADS) {
                glVertex3d(-2000, -2000, wallZ + osc1*20)
                glVertex3d(+2000, -2000, wallZ + osc2*20)
                glVertex3d(+2000, +2000, wallZ + osc3*25)
                glVertex3d(-2000, +2000, wallZ + osc4*20)
              }
            }
          }

          // Shake radiolarian
          if(firstTresenje) {
            fade1 = 0
          }
          def shake(m: Model.Model): Transform = {
            val newPos =
              if(tresenje)
                m.transform.pos + Vec(
                  (TableRandom.nextGaussian/5)  * fade1 * abs(oscillator(phi = m.phi)),
                  (TableRandom.nextGaussian/8)  * fade1 * abs(oscillator(phi = m.phi)),
                  (TableRandom.nextGaussian/15) * fade1 * abs(oscillator(phi = m.phi)))
              else
                m.transform.pos

            m.transform.copy(pos = newPos)
          }

          // Draw radiolarian
          val oscDiv = 10d
          val radiolarianSize = Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv)
          if(!pause) radiolarian.transform += radiolarian.transformVector ** renderTime
          if(firstRadioVector) {
            radiolarian.transformVector.pos = radiolarian.transformVector.pos.setZ(radioBasePosVec.z*1.75)  //((Vec0 - radiolarian.transform.pos).normalize)
          }
          
          if(firstRadioOpen) {
            fade1 = 0
          }
          
          radiolarian.transform.size = {
            val scale = 1.6
            
            (if(radioOpen) {
              (radiolarianSize * (scale * (1 - fade1))) + vec((scale + 1/oscDiv) * fade1)
            } else {
              radiolarianSize * scale
            })
          }


          val shaked: Transform = if(radioOpen) radiolarian.transform else shake(radiolarian())
          radiolarian().render(transform = shaked) // duplication below

          if(radiolarian.active) core.color -= 0.0002 // Make core go black after radiolarian opening

          core.render(transform = shaked.copy(size = radiolarian.transform.size * radiolarian.coreTransform.pos.x))

          // Draw the other radiolarians
          for(radio <- quasiradiolarians) {
            if(!pause) radio.transform += radio.transformVector ** renderTime
            radio.transform.size = radiolarianSize
            val shaked = shake(radio)
            radio.render(transform = shaked)

            if(radioVector) {
              radio.transformVector.pos = radio.transformVector.pos.setZ(radio.transformVector.pos.z*0.85)  //((Vec0 - radiolarian.transform.pos).normalize)
            }

            core.render(color = blackish, transform = shaked.copy(size = radio.transform.size * radio.coreTransform.pos.x))
          }

          // Draw rocks
          for(rock <- rocks) {
            if(!pause) rock.transform += rock.transformVector ** renderTime
            if(radioVector) {
              rock.transformVector.pos = rock.transformVector.pos.setZ(rock.transformVector.pos.z*0.85)  //((Vec0 - radiolarian.transform.pos).normalize)
            }

            rock.render(transform = shake(rock))
          } ////////////////////////////////////////////// duplication
          for(rock <- guardRocks) {
            rock.transform.setPosZ((radiolarian.transform.pos.z + rocks.head.transform.pos.z)/2)

            rock.render(transform = shake(rock))
          }
        }

        prevIzStene = izStene
        prevTresenje = tresenje
        prevRadioVector = radioVector
        prevRadioOpen = radioOpen
        prevEnd = end
        
        backBlendRender

      
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Mandalas => ////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade1 = 0
          Sound.play("mandalas")
          fade2 = 0
        }

        val div = 1.75d
        val (w, h) = (640*0.85+softHeart2*9, 800*0.85+softHeart2*9)
        val posx = winWidth/2-w/2d + rotx/div
        val posy = winHeight/2-h/2d + roty/div //don't forget the duplication below...
        var dust = false

        if(blackMandala.active) {
          glClear(0)
          quad(Coord(posx,posy, w,h), blackMandala())
          if(fade2 < 1) quad(coord2000, alpha = 1 - fade2, color = grey0)

          fade1 = 0
          //blackMandala.active = false
        } else if(whiteMandala.active) {
          val firstWhite = (whiteFlashTimer == -1)
          if(firstWhite) {
            whiteFlashTimer = now
            glClear(1)
            fade2 = 0
          } else if(since(whiteFlashTimer) <= 30*1000) { //30s
            //glClear(heart + 1-fade2)
            glClear(1-fade2)
            dust = true
          } else {
            glClear(fade2)
            beat = false
          }

          /*if(whiteMandala.cursor > 1440) {
            zoom += 1
            if(whiteMandala.cursor > 1450)
              zoom += 1
            
            quad(Coord(posx,posy, w,h) + zoom, whiteMandala())
          } else {
            quad(Coord(posx,posy, w,h), whiteMandala())
          }*/
          quad(Coord(posx,posy, w,h), whiteMandala())


          //if(fade < 1) quad(Coord(posx,posy, w,h), blackHeartDustMandala, alpha = 1-fade*2+heart/5)
        } else {
          //gotoPhase(CircleSpace)
          gotoPhase(BackSpace)
        }
        val sinceStart = since(phaseTimer)
        // 1m45s...3m  heartbeat sound and visualization
        if((sinceStart > 105*1000 || !blackMandala.active) && sinceStart < 3*60*1000) {
          if(startDustHeart == -1) startDustHeart = now

          if(beat) Sound.play("heart")

          //if(blackMandala.active) {
          if(beat) {
            if(!blackMandala.active && dust) fade2 = 0
            if(since(startDustHeart) > 15*1000 || dust) { // 15s
              val (ww, hh) = (w*0.8, h*0.8)
              val posx = winWidth/2-ww/2d + rotx/div
              val posy = winHeight/2-hh/2d + roty/div
              quad(Coord(posx,posy, ww,hh), blackHeartMandala, alpha = heart*0.7)
            }

            quad(Coord(posx,posy, w,h), blackHeartDustMandala, alpha = heart)
          }
          //}
        }

        // Linz Style
        /*if(whiteMandala.cursor > 1445 && whiteMandala.cursor < 1600) {
          fade1 = 0
        } else if(whiteMandala.cursor > 1600) {
          quad(coord2000, alpha = fade1)
        }*/
        if(whiteMandala.cursor > 1440) {
          whiteMandala.active = false
          quad(coord2000, alpha = fade1)
        } else if(whiteMandala.cursor > 1175) {
          quad(coord2000, alpha = fade1)
        } else if(whiteMandala.cursor > 1000) {
          fade1 = 0
        }

        backBlendRender

      
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case CircleSpace => /////////////////////////////////////////////////////////////////////////
        initPhase {
          fade1 = 0
          frames = 0
          stars = Vector.fill(30)(newStar)
          blackMandala.reset
          blackMandala.active = true
          //sphereMandala.reset
          sphereMandala.active = true
        }
        val radius = 400
        glClear(1)
        
        //Render.cam.lookAt(Vec3(testNum1, 0, 0))
        Render.cam.setPerspective(50, winWidth/winHeight.toFloat, 0.25f, 7000f)
        Render.cam.lookAt(Vec3(Render.cam.pos.x, Render.cam.pos.y, Render.cam.pos.z+500+testNum3))
        Render.cam.render
        render3D {
          glColor3d(1, 1, 1)
          glCapability(GL_TEXTURE_2D) {
            glBindTexture(GL_TEXTURE_2D, sphereMandala())//sphereTex)//
            glMatrix {
              glRotatef(testNum1*0-90, 1, 0, 0)
              glRotatef(testNum2*0,    0, 1, 0)
              glRotatef(testNum3*0,    0, 0, 1)
              //glScaled(testNum3*0.01+1, testNum3*0.01+1, testNum3*0.01+1)
              import org.lwjgl.util.glu.GLU
              def render() = {
                gluQuadrics.texturedSphere.draw(645+testNum1*10, 100, 100)
                //glDisable(GL_LIGHTING)
                glColor3d(1, 1, 1)
                gluQuadrics.sphere.setDrawStyle(GLU.GLU_LINE)
                gluQuadrics.sphere.draw(635+testNum1*10, 100, 100)
                ///glEnable(GL_LIGHTING)
              }
  
              render
              /*if(testNum1 != 0 || testNum2 != 0) {
              } else if(frames == 0) {
                displayListSphere = glGenLists(1)
                glNewList(displayListSphere, GL_COMPILE)
                  render
                  //gluQuadrics.sphere.draw(645, 100, 100)
                glEndList()
              } else {
                glCallList(displayListSphere)
              }*/
            }
          }
        }
        val ddd = 4
        Render.cam.rot = Vec3(-rotation.pitch/ddd, rotation.yaw/ddd, rotation.roll/ddd)

        def getVec(phi: Double, theta: Double): Vec = Vec(
          x = (cos(theta)*cos(phi)) * radius,
          y = (cos(theta)*sin(phi)) * radius,
          z = (sin(theta)) * radius)
          
        def getDiff(phi: Double, theta0: Double, theta1: Double): Vec =
          getVec(phi, theta1) - getVec(phi, theta0)
          
        def zeroDist(v: Vec): Double = v distance vec0

        /*for(magnet <- magnets) {
          val phi = magnet.phi
          val theta0 = magnet.theta
          val theta1 = magnet.theta + 0.1

          if(frames == 0) {
            magnet.transform.pos = getVec(phi, theta1)
          } else if(!pause) {

          }

          val zeroDist = magnet.transform.pos distance vec0
          if(abs(zeroDist - radius) > 0) {
            magnet.transform.pos = magnet.transform.pos * (radius/zeroDist)
          }

          magnet.render(color = color0)
        }*/

        //if(frames % 30 == 0) {
          //stars :+= newStar
        //}

        /*for(star <- stars) {
          // Great circles: http://paulbourke.net/geometry/circlesphere/
          val phi = star.phi
          val theta0 = star.theta
          val theta1 = star.theta + 0.1

          if(frames == 0) {
            star.transform.pos = getVec(phi, theta1)
          } else if(!pause) {
            val magnet = magnets.minBy { magnet => (magnet.transform.pos distance star.transform.pos) - magnet.transform.size.avg }
            val magdist = magnet.transform.pos distance star.transform.pos

            val diff = getDiff(phi, theta0, theta1)
            val magthresh = 5
            val ratio = if(magdist < magthresh) math.pow((magthresh-magdist)/magthresh, 3000) else 1
            star.transform.pos += diff*ratio + magnet.transform.pos*(1 - ratio)
            star.transformVector.pos = diff
            if(magdist < magnet.transform.size.avg && 0.01.prob) {
              star.transform.pos = Vec.random
              magnet.transform.size += Vec.randomUniform01/50
            }
            
            val zeroD = zeroDist(star.transform.pos)
            if(abs(zeroD - radius) > 0) {
              star.transform.pos = star.transform.pos * (radius/zeroD)
            }
            star.theta = theta1
            star.phi += nextDouble*0.005
          }
          star.render(color = color0)
          star.trail += star.transform.pos.copy()
          star.trail.render()
        }*/

        //quasiradiolarians.head.render(color = grey(0.5), alpha = 0.25, transform = Transform(pos = Vec0, size = Vec05))
        render3D {
          glPrimitive(GL_LINES) {
            glColor3f(1, 0, 0)
            glVertex3d(-100,    0,    0)
            glVertex3d(+100,    0,    0)
            glColor3f(0, 1, 0)
            glVertex3d(   0, -100,    0)
            glVertex3d(   0, +100,    0)
            glColor3f(0, 0, 1)
            glVertex3d(   0,    0, -100)
            glVertex3d(   0,    0, +100)
            glColor3f(0, 0, 0)
            glVertex3d(-100, -100, -100)
            glVertex3d(+100, +100, +100)
            glVertex3d(   0,    0,    0)
            glVertex3d(+100, +100, +100)
            glVertex3d(   0,    0,    0)
            glVertex3d(-100, -100, -100)
          }
        }
        
        backBlendRender

      
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case BackSpace => ///////////////////////////////////////////////////////////////////////////
        initPhase {
          fade1 = 0
          fade2 = 0
          diffStarted = false
          diffDone = false

          //backCamera.saveImage("img/Image.png")
          backPixels = Vector.empty
          phaseTimer = now
          //backCamSnap = Texture.getImage("img/Image.png")
          //backCamSnapTex = Texture("img/Image.png")
          backPixelDrop = true
          for(i <- 1 to 5) backCamera.getTextureIDWait
          Sound.play("razpad")
        }
        
        glClear(0)
        
        implicit val rpd = RenderProcessData(beat)
        
        val f = 1400d
        val (camw, camh) = (f*16/9d, f) //(winHeight*4/3d, winHeight)
        val (camx, camy) = (rotx*0.7-camw/7, roty*0.7-camh/7)

        //TODO: lock in backCamSnapTex and do a Future[blob]

        val backDrop =
          if((backPixels.isEmpty && backPixelDrop && !finished) || Keyboard.isKeyDown(Keyboard.KEY_I))
            backCamera.getTextureID
          else if(!backPixelDrop && !backPixelMerge && finished)
            backCamera.getTextureID
          else
            backCamSnapSeq()
        
        quad(Coord(camx,camy, camw,camh), backDrop, flipx = false)

        val since5 = since(phaseTimer) >= 5*1000
        val since1 = since(phaseTimer) >= 1*1000
        
        if(since5 && backPixelDrop && !finished) {
          if(!diffStarted) thread {
            diffStarted = true
            backPixels = backCamera.getDiffBlob(backCamSnap)
            val center = {
              var x, y = 0d
              for(bp <- backPixels) {
                x += bp.x
                y += bp.y
              }
              
              Vec(x/backPixels.size, y/backPixels.size, 0)
            }

            for(bp <- backPixels) {
              bp.transformVector = Vec((bp.x - center.x)/200, (bp.y - center.y)/200, 0)
            }
            Sound.play("razpadheart1")
            diffDone = true
          } else if(diffDone) {
            diffStarted = false
            diffDone = false
            backPixelDrop = false
            backPixelMerged = true
            phaseTimer = now
          }
        } else if(since5 && !backPixelDrop && !finished) {
          backPixelDrop = true
          finished = true
          phaseTimer = now
          Sound.play("razpadheart2")
          backPixelMerge = true
        } else if(since5 && backPixelMerge) {
          for(bp <- backPixels) {
            bp.transformVector = Vec((bp.sx - bp.x)/200, (bp.sy - bp.y)/200, 0)
          }
          backPixelMerged = false
          backPixelMerge = false
          phaseTimer = now
        } else if(since1 && !backPixelMerged && finished) {
          Sound.play("razpadheart1")
          backPixelMerged = true
          backPixelDrop = false
          thread { sleep(300); backPixels = Vector.empty }
        } else if(since5) {
          threadNetwork.process
        }

        //backpixelBuffer = Array.ofDim[Boolean](1920, 1080)
        glCapability(GL_BLEND) {
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
          render2D {
            glTranslated(camx, camy, 0)
            glScaled(camw/1920d, camh/1080d, 1)
            
            glColor3d(0, 0, 0)
            glPrimitive(GL_QUADS) {
              // Filter and render pixels at the same time
              backPixels = backPixels.filterNot { pixel => pixel.render(); pixel.isDead }
            }
  
            threadNetwork.render
          }
        }


        if(fade1 < 1) {
          quad(coord2000, alpha = 1-fade1)
        }

        backBlendRender

      case _ =>
    }
  }
  
  def processInput(): Unit = {
    import Keyboard._

    //if(isKeyDown(KEY_X)) Sound.play("jump")
    if(isKeyDown(KEY_1)) testNum1 -= 1
    if(isKeyDown(KEY_2)) testNum1 += 1
    if(isKeyDown(KEY_3)) testNum2 -= 1
    if(isKeyDown(KEY_4)) testNum2 += 1
    if(isKeyDown(KEY_5)) testNum3 -= 1
    if(isKeyDown(KEY_6)) testNum3 += 1
    
    if(isKeyDown(KEY_RETURN)) {
      println("Liminoid started!")
      startLiminoid = true
    }

    if(isKeyDown(KEY_Q)) rotation = rotation + Rotation(+1, 0,  0)
    if(isKeyDown(KEY_E)) rotation = rotation + Rotation(-1, 0,  0)
    if(isKeyDown(KEY_S)) rotation = rotation + Rotation(0, +1,  0)
    if(isKeyDown(KEY_W)) rotation = rotation + Rotation(0, -1,  0)
    if(isKeyDown(KEY_D)) rotation = rotation + Rotation(0,  0, +1)
    if(isKeyDown(KEY_A)) rotation = rotation + Rotation(0,  0, -1)
    if(isKeyDown(KEY_0)) { rotation = rotation0; Render.cam.pos = Vec3(0, 0, 0) }
    if(isKeyDown(KEY_UP))    Render.cam.pos.z += 1
    if(isKeyDown(KEY_DOWN))  Render.cam.pos.z -= 1
    if(isKeyDown(KEY_LEFT))  Render.cam.pos.y -= 1
    if(isKeyDown(KEY_RIGHT)) Render.cam.pos.y += 1

    if(isKeyDown(KEY_NEXT)) nextPhase
    if(isKeyDown(KEY_PRIOR)) previousPhase
    if(isKeyDown(KEY_HOME)) gotoPhase(Radiolarians)
    if(isKeyDown(KEY_END)) gotoPhase(PhaseTerminator)
    if(isKeyDown(KEY_P)) { pause = !pause; sleep(200) }

    if(isKeyDown(KEY_C)) {
      // Takes about 5 frames to set exposure, let's wait...
      for(i <- 1 to 10) backCamera.getTextureIDWait
      //backCamera.saveImage("img/Image.png")
      backPixels = Vector.empty
      phaseTimer = now
      backCamSnapSeq.rewind()
      backPixelDrop = true
      backPixelMerge = false
      backPixelMerged = true
      finished = false
      diffStarted = false
      diffDone = false
      Sound.play("razpad")
    }
    if(isKeyDown(KEY_Z)) {
      println("Making new BackSpace...")
      Sound.play("razpadheart1")
      // Takes about 5 frames to set exposure, let's wait 20...
      for(i <- 1 to 20) backCamera.getTextureIDWait
      val n = 100
      for(i <- 1 to n) {
        backCamera.saveImage(s"seq/BackSpace/$i.png")
      }
      backPixels = Vector.empty
      phaseTimer = now
      backCamSnap = Texture.getImage("seq/BackSpace/1.png")
      backCamSnapSeq.clear()
      backCamSnapSeq.preload(n)
      backCamSnapSeq.rewind()
      backCamSnapTex = Texture("seq/BackSpace/1.png")
      backPixelDrop = true
      backPixelMerge = false
      backPixelMerged = true
      finished = false
      diffStarted = false
      diffDone = false
      Sound.play("razpadheart1")
      println("BackSpace done...")
    }
    if(isKeyDown(KEY_M)) { 
      renderMode = (if(renderMode == Mono) Stereo else Mono); sleep(200) 
    }

    if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
      isMainLoopRunning = false
    }
  }
}
