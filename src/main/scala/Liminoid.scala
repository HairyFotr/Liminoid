package org.ljudmila.liminoid

import org.lwjgl.opengl.{Display,PixelFormat,DisplayMode,Util}
import org.lwjgl.input.{Keyboard, Mouse}
import collection.mutable
import collection.parallel.mutable.ParArray
import java.nio._
import scala.actors.Futures._
import scala.util.Random._
import math._
import Utils._
import org.lwjgl.opengl.{GL11,GL12,GL13,GL14}
import org.lwjgl.opengl.GL11._
import hardware.{RiftTracker,Rotation,PulseSensor}

import Model.{Transform, Vec, Vec0, Vec1, OBJModel, Color, Trail, quad, Coord}

// General tasks:
/// split setup / rendering, as to get rid of lazy vals, and things like that
/// check things for frame dependence instead of time dependence
/// check for resolution dependence (especially 2D, I think)

final object Liminoid {
  val project = "Liminoid"
  var startLiminoid = false

  sealed trait RenderMode
  case class Normal() extends RenderMode
  case class Stereo() extends RenderMode
  var renderMode: RenderMode = Stereo()

  var isMainLoopRunning = false
  var renderTime = 0d
  var lastFPS = 0f
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
    val second = 1000000000L
    val FPSseconds = 1
    var FPStimer = currentTime
    frameTime = currentTime

    isMainLoopRunning = true
    while(isMainLoopRunning) {
      processInput() // process keyboard input
      
      resetView()   // clear view and reset transformations

      renderMode match {
        case Normal() => 
          renderFrame()
          Display.update() // update window contents and process input messages
        case Stereo() =>
          shader.beginOffScreenRenderPass();
          renderFrame()  // draw stuff
          shader.endOffScreenRenderPass();
          shader.renderToScreen();
          Display.update() // update window contents and process input messages
      }
      
      frameCounter += 1

      if(currentTime-FPStimer > second*FPSseconds) {
        val FPS = frameCounter/FPSseconds.toFloat

        println("FPS: "+FPS)
        println("testNum1: "+testNum)
        println("testNum2: "+testNum2)
        println("testNum3: "+testNum3)
        println("sinceStart: "+since(phaseTimer))
        println("radiolarian: "+radiolarian.transform)
        println("rotation:"+rotation)

        lastFPS = FPS
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
    glClearColor(0,0,0,1)

    glEnable(GL_DEPTH_TEST) // enable depth buffer (off by default)
    //glEnable(GL_CULL_FACE)  // enable culling of back sides of polygons
    //glCullFace(GL_BACK)
    
    // smooth shading - Gouraud
    glShadeModel(GL_SMOOTH)
    //glShadeModel(GL_FLAT)

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
  def initPhase(f: => Unit): Unit = if(phaseChanged) {
    phaseTimer = now
    phaseChanged = false
    f
    System.gc() //best place to do it...
  }

  // Cameras
  //val cams = Array(hardware.Camera(camId = 0, 1920, 1080), hardware.Camera(camId = 1, 1920, 1080), hardware.Camera(camId = 2, 1280, 720), hardware.Camera(camId = 3, 1280, 720))
  //val stereoCameras = cams.takeRight(2).reverse
  val backCamera = hardware.Camera(camId = 1, 1920, 1080) //cams(0)
  val frontCamera = hardware.Camera(camId = 0, 1280, 720)

  val eyeCorrection = -64 // Eye shift for 2D rift view
  var testNum = 0;
  var testNum2 = 0;
  var testNum3 = 0;

  /// Radiolarians phase objects ///
  val white = Color(0.9)
  val wallZ = 600 // z position of wall
  var radioBasePosVec = Vec(0,0,-0.11) // basic z movement vector
  val startPos = Vec(0,0,wallZ+15) // central starting point
  def basicRot(): Vec = Vec.random11/3
  
  // The rock inside radiolarians
  lazy val core = OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(color = Color(0.2,0.2,0.2))

  // The radiolarian that opens up
  lazy val radiolarian = 
    OBJSequence(
      path = "obj/OBJ_the_radiolarian_normale", 
      active = false,
      stopAtEnd = true,
      delay = 80,
      color = white,
      coreTransform = Transform(pos = Vec(3,3,3)),
      transform = Transform(pos = startPos + Vec(0,0,-5)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec0))
    
  // The other radiolaria
  lazy val quasiradiolarians = 
    Array(
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VII.obj").toModel( // holes
        transform = Transform(pos = startPos + Vec(-45,21,2), size = Vec1*0.6),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white),
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VII.obj").toModel( // holes
        transform = Transform(pos = startPos + Vec(25,-19,9), size = Vec1*0.7),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white),
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_V.obj").toModel( // Same as radiolarian
        transform = Transform(pos = startPos + Vec(48,-33,5), size = Vec1*0.9),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(3,3,3)),
        color = white), //majhen
      //OBJModel("obj/Ogromni_modeli/Plascek_normale_IV.obj").toModel( // Same ??
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_V.obj").toModel(
        transform = Transform(pos = startPos + Vec(30,25,5), size = Vec1*0.9),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(3,3,3)),
        color = white), //majhen
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VI.obj").toModel( // Edgy one
        transform = Transform(pos = startPos + Vec(-53,-18,9), size = Vec1*0.7),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white),
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VI.obj").toModel( // Edgy one
        transform = Transform(pos = startPos + Vec(-15,-24,8), size = Vec1*0.6),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white)//,
      /*OBJModel("obj/Plascki_iz_stene/Plascek_normale_VII.obj").toModel( // Big holes
        transform = Transform(pos = startPos + Vec(35,15,8), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white)*///,
      /*OBJModel("obj/Plascki_iz_stene/Plascek_normale_IX_mali.obj").toModel( //high-poly version: obj/Ogromni_modeli/Plascek_normale_IX_velik.obj
        transform = Transform(pos = startPos + Vec(-15,37,63), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(3,3,3)),
        color = white)*///, //test it
      /*OBJModel("obj/Plascki_iz_stene/Plascek_normale_IX_mali.obj").toModel( //The thick one
        transform = Transform(pos = startPos + Vec(-5,-14,82), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
        coreTransform = Transform(pos = Vec(3,3,3)),
        color = white)*/)

  // Some rocks just floating around
  lazy val rocks = Array(
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
      transform = Transform(pos = startPos + Vec(-25,22,12), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel(
      transform = Transform(pos = startPos + Vec(-40,-4,4), size = Vec(4,4,4)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(-67,18,1), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),///////////////////////////////////////////////////////////////////duplication
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel(
      transform = Transform(pos = startPos + Vec(-75,-27,13), size = Vec(4,4,4)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(-35,-35,17), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel( // I ima kocko in prevec vertexov
      transform = Transform(pos = startPos + Vec(35,12,11), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(48,5,8), size = Vec(2.6,2.6,2.6)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
      transform = Transform(pos = startPos + Vec(72,-15,12), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
      transform = Transform(pos = startPos + Vec(65,-45,15), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(4,35,12), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white))

  // rocks that fly with the radiolarian
  lazy val guardRocks = Array(
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(-29,-13,0), size = Vec(2.2,2.2,2.2)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(7,-32,0), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = basicRot),
      color = white))

  /// Mandalas phase objects ///
  val blackMandala = TexSequence("seq/optipng_Sekvenca_mandala_crno_ozadje", delay = 1000/24d, stopAtEnd = true, selfDestruct = true)
  val whiteMandala = TexSequence("seq/optipng_Sekvenca_mandala_belo_ozadje", delay = 1000/24d, stopAtEnd = true, selfDestruct = true)
  var whiteFlashTimer = -1
  var startHeart = -1
  var startDustHeart = -1
  var lastHeartpos = 0d
  lazy val blackHeartMandala = Texture("seq/Srcni_utrip_CO/Srcni_utrip_CO_01290.png")
  lazy val blackHeartDustMandala = Texture("seq/Srcni_utrip_CO_II/Srcni_utrip_CO_II_01287.png")
  lazy val whiteHeartMandala = Texture("seq/Srcni_utrip_BO/Srcni_utrip_BO_05848.png")
  var zoom = 0d

  /// CircleSpace phase objects ///
  def newStar(): Model.Model = OBJModel("obj/UV_sfera/UV_sfera_I.obj").toModel(
    transform = Transform(rot = Vec.random, size = (Vec1/2) + (Vec.random/3)),
    transformVector = Transform(rot = Vec.random),
    color = Color(0.2,0.2,0.2),
    phi = nextDouble*math.Pi*2, theta = nextDouble*math.Pi*2)
  var stars = Vector.empty[Model.Model]

  lazy val magnets =
    Vector.fill(15)(
      OBJModel("obj/UV_sfera/UV_sfera_I.obj").toModel(
        transform = Transform(rot = Vec.random, size = Vec1*2),
        transformVector = Transform(rot = Vec.random),
        color = Color(0.3,0.3,0.3),
        phi = nextDouble*math.Pi*2, theta = nextDouble*math.Pi*2))

  // BackSpace phase objects
  var wall = -1
  var backCamSnap = Texture.getImage("img/Image.png")
  var backCamSnapTex = backCamera.getTextureIDWait
  var backPixels = Vector.empty[Model.Pixel]
  var backpixelBuffer = Array[Array[Boolean]]()
  var backPixelDrop = false
  var finished = false

  // variable that goes to 1 by little each frame
  var fade = 1d
  var fadeSpeed = 0.002
  var fade2 = 1d
  var fadeSpeed2 = 0.04

  // have a small bump for movement
  val shakeBump = 3
  val shakeBumpN = 50


  // Oculus rift head tracking
  var lastRotation = Rotation(0,0,0)
  var rotation = Rotation(0,0,0)
  
  var lastIzStene = false
  var lastTresenje = false
  var lastRadioVector = false
  var lastRadioOpen = false
  var lastEnd = false

  var frames = 0L
  def renderFrame(): Unit = {
    frames += 1

    /*
      linux -> arduino

      prehod med fazami
      setup -> heartbeat works, rift view is forward, images preloaded ->
      radiolarians -> radiolarian comes close, opens, black screen ->
      mandalas -> the whole thing plays ->
      circlespace -> utrinek comes close and flashes ->
      backspace -> 

      reorganization
        start opengl, then go onto app -> get rid of some lazy loading
      optimizations,ideas,
        preload images while fps >= 60
      details
        call camtex when switching to a camera phase
        faint rift tracking at mandalas
        menger sponge feathered texture on radiolarians!!!

      realne koordinate stene
      senca na steni
      floaterji so bolj na mestu
      radiolarians gredo na pogled proti tebi
      radiolarian s kuglo

      mandale
      mora bit posyncano
      pulz opacity layers

      cameras
        2x Logitech c270 hacking  
          Bus 003 Device 013: ID 046d:0825 Logitech, Inc. Webcam C270
          Bus 003 Device 012: ID 046d:0825 Logitech, Inc. Webcam C270
          Focus hacking http://www.youtube.com/watch?v=v-gYgBeiOVI

        1x Logitech c615
          Bus 003 Device 014: ID 046d:082c Logitech, Inc. 
          http://www.linux-hardware-guide.com/2013-04-20-logitech-hd-webcam-c615

        add timeout to camera to prevent old frames

      to white, mogoce ze prej vletavajo utrinki "asteroid field effect"

      kugla z uv mapo kamere, upocasnitev pogleda

      kamera
        two cameras - figure out the spacing, FOV, etc
        one camera - move image for each eye

      gl
        glClear
        glBegin(GL_SOMETHING) {
  
        }
        glPush {
  
        }


      OS support
        opencv je zaeenkrat treba rocno... obstaja sbt string?
        lib rift sem sam scompilal da sploh ne zaznava Rifta
        PNGDecoder pride z lwjgl oz. kje je sbt string?
        jlplayer
      
    */
    
    def glClear(g: Double): Unit = {
      GL11.glClearColor(g.toFloat,g.toFloat,g.toFloat,1)
      GL11.glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    }
    def drawFrontCam(): Unit = {
      val img = frontCamera.getTextureID
      val camScale = -1030
      val camhCorrect = 650
      val (camw, camh) = (winHeight*16/9d, winHeight+camhCorrect)
      val (camx, camy) = (winWidth/2-camw/2, -camhCorrect/2)
      quad(Coord(camx,camy,camw,camh)+camScale, img, alpha = 1, flipy = true, flipx = true)
    }


    // Generate/Get oscilators, and heart signals
    var beat = PulseSensor.takeBeat
    if(!PulseSensor.init_) beat = (frames % 70) == 0
    val heart = if(beat) 1d else 0d
    
    /*val harhar = 20
    var beat = (frames % 70) == 0
    val heart = (frames % 70) match {
      case 0 => 1
      case x if x < harhar => pow(0.6, x)
      case x if x > harhar => pow(0.75, 70-x)
      case _ => 0
    }*/

    // http://www.dspguru.com/dsp/howtos/how-to-create-oscillators-in-software
    // f = desired oscillator frequency
    // w = 2 pi f / Fs 
    // phi = initial phase
    // y = ouput array containing the oscillator signal
    //def oscillator(i: Double) = sin((2*Pi*f)*i + phi)

    //def oscillator(i: Double, phi: Double) = sin(i + phi)
    def oscillator(i: Double = Utils.now*0.002, phi: Double): Double = sin(i + phi)

    val osc1 = oscillator(phi = 0)
    val osc2 = oscillator(phi = 1*Pi/4)
    val osc3 = oscillator(phi = 2*Pi/4)
    val osc4 = oscillator(phi = 3*Pi/4)

    val (mw, mh) = (200-osc1*100-osc3*30, 160-osc2*50-osc3*20)
    val (cx, cy) = (winWidth/2 - mw/2, winHeight/2 - mh/2)
    val ratio = mw/mh.toFloat

    // Get head rotation 2d, 3d
    val (rotx, roty) = (
      if(lastRotation == Rotation(0,0,0)) {
        lastRotation = RiftTracker.poll
        
        (0f,0f)
      } else {
        val rot = RiftTracker.poll
        val rotDelta = rot - lastRotation
        lastRotation = rot*0.1f + lastRotation*0.9f
        rotation = rotation + rotDelta
        
        (-rotation.yaw, -rotation.pitch)
      })

    if(fade < 1) fade += fadeSpeed else fade = 1
    if(fade2 < 1) fade2 += fadeSpeed2 else fade2 = 1

    phase match {
      case Setup => /////////////////////////////////////////////////////////////////////////////////////////////
        glClear(0)

        //Mandala debug
        //blackMandala.preload(50); whiteMandala.preload(50)
        //blackHeartMandala; blackHeartDustMandala; whiteHeartMandala; println("Mandalas preloaded")
        //gotoPhase(Radiolarians)
        //.............

        val (camw, camh) = (winHeight*4/3d, winHeight)
        val (camx, camy) = (winWidth/2-camw/2, 0)
        quad(Coord(winWidth/2-940/2,winHeight/2-550/2,940,550), liminoidTitle, alpha = 1)

        // Triggers the lazy compute or preload sequence
        println("Time" + (frames-1) + ": " + Utils.time((frames-1) match {
          case 0 => 
          case 1 => 
          case 2 => radiolarian; println("Radiolarians1 loaded")
          case 3 => quasiradiolarians; println("Radiolarians2 loaded")
          case 4 => rocks; guardRocks; println("Rocks loaded")
          case 5 => blackMandala.preload(50); whiteMandala.preload(50)
          case 6 => blackHeartMandala; blackHeartDustMandala; whiteHeartMandala; println("Mandalas preloaded")
          case 7 => //wall
          case 8 => //magnets
          case 9 => //startLiminoid = true
          case _ => if(startLiminoid) gotoPhase(Radiolarians) else Thread.sleep(100)
        }))
        
        System.gc()
        System.gc() // JVM, just do it, please

      case -1 => /////////////////////////////////////////////////////////////////////////////////////////////
        //Limbo phase :)
        glClear(1)

        drawFrontCam()

        // TODO stereo cam
        //val (camw, camh) = (winWidth, winHeight)
        //val (camx, camy) = (winWidth/2+528+Model.cam.pos.y, -388+Model.cam.pos.z)
        //println(Model.cam.pos)

        //Model.quadStereo(Coord(camx,camy,camw,camh)+testNum, stereoCameras(0).getTextureID, stereoCameras(1).getTextureID, alpha = 1, rotate90 = true, flipy = true, flipx = true)


      case Radiolarians => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          Sound.play("intro")
        }
        //
        // senzor magnetometer absolute or rolling average during loading phase
        //1
        /// //Postavi objekte glede na pleksi
        /// Pocasen napad glede na muziko
        /// ob <t> nastavi vektor in kot proti uporabniku
        //
        //2
        /// Pulse sensor
        /// Zoom <n> seconds before end (at circle)
        //
        //3 
        /// Fade white to camera
        /// Tweakanje
        //

        // According to scenario
        val izSteneTimeProper = (17)*1000
        val tresenjeTimeProper = (35)*1000
        val radioVectorTimeProper = (60+25)*1000
        val radioOpenTimeProper = (60+43)*1000 // Fall down?
        val endTimeProper = (120+8)*1000
        
        val test = false
        val testTime = 1000

        val izSteneTime = if(test) 0 else izSteneTimeProper
        var izStene = since(phaseTimer) > izSteneTime
        val firstIzStene = (!lastIzStene && izStene) 

        // Start shaking elements
        val tresenjeTime = if(test) izSteneTime + testTime else tresenjeTimeProper
        val tresenje = since(phaseTimer) > tresenjeTime
        val firstTresenje = (!lastTresenje && tresenje) 

        // Pull radiolarian towards view
        val radioVectorTime = if(test) tresenjeTime + testTime else radioVectorTimeProper
        val radioVector = since(phaseTimer) > radioVectorTime
        val firstRadioVector = (!lastRadioVector && radioVector) 

        // Activate radiolarian shell open animation
        val radioOpenTime = if(test) radioVectorTime + testTime else radioOpenTimeProper
        val radioOpen = since(phaseTimer) > radioOpenTime
        if(radioOpen) {
          radiolarian.active = true; 
        }
        val firstRadioOpen = (!lastRadioOpen && radioOpen) 

        // End phase
        val endTime = if(test) radioOpenTime + testTime else endTimeProper
        val end = since(phaseTimer) > endTime
        if(end) {
        }
        val firstEnd = (!lastEnd && end) 

        if((radiolarian.transform.pos distance Vec(0,0,0)) < 1 || radiolarian.transform.pos.z < -1) {
          gotoPhase(Mandalas)
        }
        glClear(0)
        drawFrontCam()
        
        // Lines
        //val clr = 0.75 + 0.25 * nextDouble
        //for(y <- 0 to 10) quad(Coord(0,camy,10000,2), color = Color(clr, clr, clr), alpha = 0.8)

        radiolarian.transformVector.rot = Vec0
        radiolarian.transform.rot = Vec(90,0,0)

        radiolarian.transform.setPosZ(radiolarian.transform.pos.z + testNum*5)
        testNum = 0
        radiolarian.transform.setPosX(radiolarian.transform.pos.x + testNum2*5)
        testNum2 = 0
        radiolarian.transform.setPosY(radiolarian.transform.pos.y + testNum3*5)
        testNum3 = 0

        // Render Camera
        Model.cam.lookAt(Vec3(0,0,500))
        Model.cam.render
        val rotationCalibration = 4 + (testNum+700)/100f
        Model.cam.rot = Vec3(-rotation.pitch/rotationCalibration, rotation.yaw/rotationCalibration, rotation.roll/rotationCalibration)

        if(izStene) {
          // Draw invisible wall
          Model.render3D {
            glEnable(GL_DEPTH_TEST)
            glEnable(GL_BLEND)
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
            glColor4f(1,1,1,0)
            glBegin(GL_QUADS)
              glVertex3d(-1000, -1000, wallZ)
              glVertex3d(+1000, -1000, wallZ)
              glVertex3d(+1000, +1000, wallZ)
              glVertex3d(-1000, +1000, wallZ)
            glEnd()
            glDisable(GL_DEPTH_TEST)
            glDisable(GL_BLEND)
          }

          if(firstTresenje) {
            fade = 0
          }
          def shake(m: Model.Model): Transform = {
            val newPos = 
              if(tresenje) // && ((frames % shakeBumpN) > shakeBumpN/3d))
                m.transform.pos
                  .setX(m.transform.pos.x + (nextGaussian/5)*fade*abs(oscillator(phi = m.phi)))
                  .setY(m.transform.pos.y + (nextGaussian/8)*fade*abs(oscillator(phi = m.phi)))
                  .setZ(m.transform.pos.z + (nextGaussian/15)*fade*abs(oscillator(phi = m.phi)))
              else 
                m.transform.pos

            m.transform.copy(pos = newPos)
          }

          // Draw radiolarian
          val oscDiv = 10d
          if(!pause) radiolarian.transform += radiolarian.transformVector ** renderTime
          if(firstRadioVector) {
            radiolarian.transformVector.pos = radiolarian.transformVector.pos.setZ(radioBasePosVec.z*1.75)  //((Vec(0,0,0) - radiolarian.transform.pos).normalize)
          }
          
          val s = 1.6
          if( firstRadioOpen) {
            fade = 0
          }
          radiolarian.transform.size = 
            (if(radioOpen) {
              (((Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv) * s) * (1 - fade)) + (Vec(s+1/oscDiv,s+1/oscDiv,s+1/oscDiv) * fade))
            } else {
              Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv) * s
            })


          val shaked: Transform = if(radioOpen) radiolarian.transform else shake(radiolarian())
          radiolarian().render(transform = shaked) // duplication below

          if(radiolarian.active) core.color -= 0.0002 // Make core go black after radiolarian opening

          core.render(transform = shaked.copy(size = radiolarian.transform.size * radiolarian.coreTransform.pos.x))

          // Draw the other radiolarians
          for(radio <- quasiradiolarians) {
            if(!pause) radio.transform += radio.transformVector ** renderTime
            radio.transform.size = Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv)
            val shaked = shake(radio)
            radio.render(transform = shaked)

            if(radioVector) {
              radio.transformVector.pos = radio.transformVector.pos.setZ(radio.transformVector.pos.z*0.85)  //((Vec(0,0,0) - radiolarian.transform.pos).normalize)
            }

            core.render(color = Color(0.15, 0.15, 0.15), transform = shaked.copy(size = radio.transform.size * radio.coreTransform.pos.x))
          }

          // Draw rocks
          for(rock <- rocks) {
            if(!pause) rock.transform += rock.transformVector ** renderTime
            if(radioVector) {
              rock.transformVector.pos = rock.transformVector.pos.setZ(rock.transformVector.pos.z*0.85)  //((Vec(0,0,0) - radiolarian.transform.pos).normalize)
            }

            rock.render(transform = shake(rock))
          } ////////////////////////////////////////////// duplication
          for(rock <- guardRocks) {
            rock.transform.setPosZ((radiolarian.transform.pos.z + rocks.head.transform.pos.z)/2)

            rock.render(transform = shake(rock))
          }
        }

        lastIzStene = izStene
        lastTresenje = tresenje
        lastRadioVector = radioVector
        lastRadioOpen = radioOpen
        lastEnd = end

      case Mandalas => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade = 0
          Sound.play("mandalas")
          fade2 = 0
        }

        //if(beat) Sound.play("heart")

        val div = 1.75d
        val (w, h) = (640*0.85, 800*0.85)
        val posx = winWidth/2d-w/2d + rotx/div
        val posy = winHeight/2d-h/2d + roty/div //don't forget the duplication below...
        var dust = false

        if(blackMandala.active) {
          glClear(0)
          quad(Coord(posx,posy, w, h), blackMandala(), alpha = 1)
          if(fade2 < 1) quad(Coord(0,0,2000,2000), alpha = 1 - fade2, color = Color(0,0,0))

          fade = 0
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

          if(whiteMandala.cursor > 1445) zoom += 2

          quad(Coord(posx,posy, w, h) + zoom, whiteMandala(), alpha = 1)

          //if(fade < 1) quad(Coord(posx,posy, w, h), blackHeartDustMandala, alpha = 1-fade*2+heart/5)
        } else {
          //gotoPhase(CircleSpace)
          gotoPhase(BackSpace)
        }
        val sinceStart = since(phaseTimer)
        if((sinceStart > 105*1000 || !blackMandala.active) && sinceStart < 3*60*1000) {//1m45s...3m  heartbeat sound and visualization
          if(startDustHeart == -1) startDustHeart = now

          if(beat) Sound.play("heart")

          //if(blackMandala.active) {
          if(beat) {
            if(!blackMandala.active && dust) fade2 = 0
            if(since(startDustHeart) > 15*1000 || dust) {//15s
              val (ww, hh) = (w*0.8, h*0.8)
              val posx = winWidth/2d-ww/2d + rotx/div
              val posy = winHeight/2d-hh/2d + roty/div
              quad(Coord(posx,posy, ww, hh), blackHeartMandala, alpha = heart*0.7)
              println("wat")
            }

            quad(Coord(posx,posy, w, h), blackHeartDustMandala, alpha = heart)
          }
          //}
        }

        if(whiteMandala.cursor > 1445 && whiteMandala.cursor < 1600) {
          fade = 0
        } else if(whiteMandala.cursor > 1600) {
          glColor3f(1,1,1)
          quad(Coord(0,0,2000,2000), alpha = fade)
        }

        GL14.glBlendEquation(GL14.GL_FUNC_REVERSE_SUBTRACT)
        quad(Coord(0,0, winWidth, winHeight) + zoom, color = Color(0,0.125,0.25), alpha = 0.1, blend = (GL_ONE, GL_ONE))
        GL14.glBlendEquation(GL14.GL_FUNC_ADD)

      case CircleSpace => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade = 0
          frames = 0
          stars = Vector.fill(30)(newStar)
        }
        val radius = 400
        glClear(1)

        //Model.cam.lookAt(Vec3(testNum,0,0))
        Model.cam.lookAt(Vec3(0,0,500))
        Model.cam.render
        val ddd = 4
        Model.cam.rot = Vec3(-rotation.pitch/ddd, rotation.yaw/ddd, rotation.roll/ddd)

        def getVec(phi: Double, theta: Double): Vec = Vec(
          x = (cos(theta)*cos(phi)) * radius,
          y = (cos(theta)*sin(phi)) * radius,
          z = (sin(theta)) * radius)
        def getDiff(phi: Double, theta0: Double, theta1: Double): Vec = getVec(phi,theta1) - getVec(phi,theta0)
        def zeroDist(v: Vec): Double = v distance Vec(0,0,0)

        for(magnet <- magnets) {
          val phi = magnet.phi
          val theta0 = magnet.theta
          val theta1 = magnet.theta + 0.1

          if(frames == 0) {
            magnet.transform.pos = getVec(phi, theta1)
          } else if(!pause) {

          }

          val zeroDist = magnet.transform.pos distance Vec(0,0,0)
          if(abs(zeroDist - radius) > 0) {
            magnet.transform.pos = magnet.transform.pos * (radius/zeroDist)
          }

          magnet.render(color = Color(0,0,0))
        }

        if(frames % 30 == 0) {
          //stars :+= newStar
        }

        for(star <- stars) {
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
            if(magdist < magnet.transform.size.avg && nextDouble < 0.01) {
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
          star.render(color = Color(0,0,0))
          star.trail += star.transform.pos.copy()
          star.trail.render()
        }

        //quasiradiolarians.head.render(color = Color(0.5, 0.5, 0.5), alpha = 0.25, transform = Transform(pos = Vec(0,0,0), size = Vec(0.5,0.5,0.5)))
        Model.render3D {
          glBegin(GL_LINES)
            glColor3f(1,0,0)
            glVertex3d(-100,0,0)
            glVertex3d(+100,0,0)
            glColor3f(0,1,0)
            glVertex3d(0,-100,0)
            glVertex3d(0,+100,0)
            glColor3f(0,0,1)
            glVertex3d(0,0,-100)
            glVertex3d(0,0,+100)
            glColor3f(0,0,0)
            glVertex3d(-100,-100,-100)
            glVertex3d(+100,+100,+100)
            glVertex3d(0,0,-0)
            glVertex3d(+100,+100,+100)
            glVertex3d(0,0,0)
            glVertex3d(-100,-100,-100)
          glEnd
        }


      case BackSpace => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade = 0

          fade2 = 0

          //backCamera.saveImage("img/Image.png");
          backPixels = Vector.empty;
          phaseTimer = now
          backCamSnap = Texture.getImage("img/Image.png")
          backCamSnapTex = Texture("img/Image.png")//backCamera.getTextureIDWait
          backPixelDrop = true
          Sound.play("razpad")
        }
        
        glClear(0)

        val f = 1400d
        val (camw, camh) = (f*16/9d, f) //(winHeight*4/3d, winHeight)
        val (camx, camy) = (rotx*0.7-camw/7, roty*0.7-camh/7)

        backCamera.getTextureID

        if((backPixels.isEmpty && backPixelDrop == true && !finished) || Keyboard.isKeyDown(Keyboard.KEY_I)) 
          quad(Coord(camx,camy,camw,camh), backCamera.getTextureID, alpha = 1, flipx = false)
        else
          quad(Coord(camx,camy,camw,camh), backCamSnapTex, alpha = 1, flipx = false)

        if((backPixelDrop == true && since(phaseTimer) >= 15*1000 && !finished) || Keyboard.isKeyDown(Keyboard.KEY_I)) {
          //if(backPixels.isEmpty || Keyboard.isKeyDown(Keyboard.KEY_I)) {
          val blob = backCamera.getDiffBlob(backCamSnap)
          backPixels = blob //++ blob.map { _.copy() } ++ blob.map { _.copy() }
          //backCamera.saveImage("img/Image.png"); backCamSnap = Texture.getImage("img/Image.png"); wall = Texture("img/Image.png"); 
          val cv = {
            var x,y = 0d
            backPixels foreach { b => x += b.x; y += b.y }
            
            Vec(x/backPixels.size, y/backPixels.size, 0)
          }

          backPixels.foreach { b => b.transformVector = (Vec(b.x, b.y, 0) - cv)/200 }
          Sound.play("razpadheart1")


          backPixelDrop = false
          phaseTimer = now
          //}
        }
        if(backPixelDrop == false && since(phaseTimer) >= 15*1000) {
          backPixelDrop = true
          finished = true
          phaseTimer = now
          Sound.play("razpadheart2")
        }

        //backpixelBuffer = Array.ofDim[Boolean](1920, 1080)
        Model.render2D {
          glTranslated(camx, camy, 0)
          glScaled(camw/1920d, camh/1080d, 1)
          glColor4d(0, 0, 0, 1)
          //glBegin(GL_POINTS)
          glBegin(GL_QUADS)
          backPixels.foreach(_.render())
          glEnd
        }
        backPixels = backPixels.filterNot(_.dead)

        if(fade < 1) {
          glColor3f(1,1,1)
          quad(Coord(0,0,2000,2000), alpha = 1-fade)
        }

      case _ =>
    }
  }
  
  def processInput(): Unit = {
    import Keyboard._

    if(isKeyDown(KEY_X)) Sound.play("jump")
    if(isKeyDown(KEY_1)) testNum -= 1
    if(isKeyDown(KEY_2)) testNum += 1
    if(isKeyDown(KEY_3)) testNum2 -= 1
    if(isKeyDown(KEY_4)) testNum2 += 1
    if(isKeyDown(KEY_PERIOD)) testNum3 -= 1
    if(isKeyDown(KEY_COMMA))  testNum3 += 1
    
    if(isKeyDown(KEY_RETURN)) {
      println("Liminoid started!")
      startLiminoid = true
    }

    /*if(isKeyDown(KEY_W)) Model.cam.pos.z += 1
    if(isKeyDown(KEY_S)) Model.cam.pos.z -= 1
    if(isKeyDown(KEY_D)) Model.cam.pos.x -= 1
    if(isKeyDown(KEY_A)) Model.cam.pos.x += 1
    if(isKeyDown(KEY_Q)) Model.cam.pos.y -= 1
    if(isKeyDown(KEY_E)) Model.cam.pos.y += 1
    if(isKeyDown(KEY_0)) Model.cam.pos = Vec3(0,0,0)*/

    if(isKeyDown(KEY_Q)) rotation = rotation + Rotation(+1,0,0)
    if(isKeyDown(KEY_E)) rotation = rotation + Rotation(-1,0,0)
    if(isKeyDown(KEY_S)) rotation = rotation + Rotation(0,+1,0)
    if(isKeyDown(KEY_W)) rotation = rotation + Rotation(0,-1,0)
    if(isKeyDown(KEY_D)) rotation = rotation + Rotation(0,0,+1)
    if(isKeyDown(KEY_A)) rotation = rotation + Rotation(0,0,-1)
    if(isKeyDown(KEY_0)) { rotation = Rotation(0,0,0); Model.cam.pos = Vec3(0,0,0) }
    if(isKeyDown(KEY_UP))    Model.cam.pos.z += 1
    if(isKeyDown(KEY_DOWN))  Model.cam.pos.z -= 1
    if(isKeyDown(KEY_LEFT))  Model.cam.pos.y -= 1
    if(isKeyDown(KEY_RIGHT)) Model.cam.pos.y += 1

    /*if(isKeyDown(KEY_SPACE)) println(modelSeq)
    if(isKeyDown(KEY_UP))    modelSeq.pos = modelSeq.pos.copy(z = modelSeq.pos.z + 1)
    if(isKeyDown(KEY_DOWN))  modelSeq.pos = modelSeq.pos.copy(z = modelSeq.pos.z - 1)
    if(isKeyDown(KEY_LEFT))  modelSeq.pos = modelSeq.pos.copy(x = modelSeq.pos.x + 1)
    if(isKeyDown(KEY_RIGHT)) modelSeq.pos = modelSeq.pos.copy(x = modelSeq.pos.x - 1)

    if(isKeyDown(KEY_W)) modelSeq.rot = modelSeq.rot.copy(z = modelSeq.rot.z + 2)
    if(isKeyDown(KEY_S)) modelSeq.rot = modelSeq.rot.copy(z = modelSeq.rot.z - 2)
    if(isKeyDown(KEY_A)) modelSeq.rot = modelSeq.rot.copy(y = modelSeq.rot.y + 2)
    if(isKeyDown(KEY_D)) modelSeq.rot = modelSeq.rot.copy(y = modelSeq.rot.y - 2)
    if(isKeyDown(KEY_Q)) modelSeq.rot = modelSeq.rot.copy(x = modelSeq.rot.x + 2)
    if(isKeyDown(KEY_E)) modelSeq.rot = modelSeq.rot.copy(x = modelSeq.rot.x - 2)*/

    if(isKeyDown(KEY_NEXT)) nextPhase
    if(isKeyDown(KEY_PRIOR)) previousPhase
    if(isKeyDown(KEY_HOME)) gotoPhase(Radiolarians)
    if(isKeyDown(KEY_END)) gotoPhase(PhaseTerminator)
    if(isKeyDown(KEY_P)) { pause = !pause; }

    if(isKeyDown(KEY_C)) { 
      backCamera.getTextureIDWait;
      //backCamera.saveImage("img/Image.png");
      backPixels = Vector.empty;
      phaseTimer = now
      backCamSnap = Texture.getImage("img/Image.png")
      backCamSnapTex = Texture("img/Image.png")//backCamera.getTextureIDWait
      backPixelDrop = true
      finished = false
    }
    if(isKeyDown(KEY_Z)) { 
      backCamera.getTextureIDWait
      backCamera.saveImage("img/Image.png")
      backPixels = Vector.empty
      phaseTimer = now
      backCamSnap = Texture.getImage("img/Image.png")
      backCamSnapTex = Texture("img/Image.png")//backCamera.getTextureIDWait
      backPixelDrop = true
      finished = false
    }
    if(isKeyDown(KEY_M)) { renderMode = (if(renderMode == Normal()) Stereo() else Normal()) }

    if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
      isMainLoopRunning = false
    }    
  }
}
