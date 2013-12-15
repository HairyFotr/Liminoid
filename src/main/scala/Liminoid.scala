package org.ljudmila.liminoid

import org.lwjgl.opengl.{Display,PixelFormat,DisplayMode,Util}
import org.lwjgl.input.{Keyboard, Mouse}
import collection.mutable.{HashMap,HashSet,ListBuffer,LinkedHashMap}
import collection.parallel.mutable.ParArray
import java.nio._
import scala.actors.Futures._
import scala.util.Random._
import math._
import Utils._
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL12
import org.lwjgl.opengl.GL13
import org.lwjgl.opengl.GL11._
import hardware.{RiftTracker,Rotation}

import Model.{Transform, Vec, Vec0, Vec1, OBJModel, Color, Trail, quad, Coord}

// General tasks:
/// split setup / rendering, as to get rid of lazy vals, and things like that
/// check things for frame dependence instead of time dependence
/// check for resolution dependence (especially 2D, I think)

final object Liminoid {
  val project = "Liminoid"

  sealed trait RenderMode
  case class Normal() extends RenderMode
  case class Stereo() extends RenderMode
  var renderMode: RenderMode = Stereo()

  var isMainLoopRunning = false
  var renderTime = 0f
  var lastFPS = 0f
  var pause = false

  /**
   * Initializes display and enters main loop
   */
  def main(args: Array[String]) {
    initDisplay()
    Sound.init()
    RiftTracker.init()

    mainLoop()
    
    // Cleanup
    Sound.stopAll()
    RiftTracker.destroy()
    Display.destroy()
  }

  var (winWidth, winHeight) = (1920, 1080)
  val (forceWidth, forceHeight) = (1920, 1080)
  def initDisplay() {
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
  def mainLoop() {
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
        println("radiolarian: "+radiolarian.transform)
        println("rotation:"+rotation)

        lastFPS = FPS
        frameCounter = 0
        FPStimer = currentTime
      }

      renderTime = (currentTime-frameTime)
      frameTime = currentTime
    }
  }
  
  /**
  * Initial setup of projection of the scene onto screen, lights etc.
  */
  def setupView() {
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

    glLight(GL_LIGHT0, GL_AMBIENT, floatBuffer(0.5f, 0.5f, 0.5f, 0.0f))
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
  def resetView() {
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
  
  def gotoPhase(i: Int) {
    phase = i
    phaseChanged = true
  }
  def nextPhase() {
    if(phaseChanged || phase >= PhaseTerminator) return
    phase += 1
    phaseChanged = true
  }
  def previousPhase() {
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
  val cams = Array(hardware.Camera(camId = 0, 1920, 1080), hardware.Camera(camId = 1), hardware.Camera(camId = 2, 1280, 720), hardware.Camera(camId = 3, 1280, 720))
  val backCamera = cams.head
  val stereoCameras = cams.takeRight(2).reverse

  val eyeCorrection = -64 // Eye shift for 2D rift view
  var testNum = 0;
  var testNum2 = 0;
  var testNum3 = 0;

  /// Radiolarians phase objects ///
  val white = Color(0.85)
  val wallZ = 600 // z position of wall
  var radioBasePosVec = Vec(0,0,-0.1) // basic z movement vector
  val startPos = Vec(0,0,wallZ+15) // central starting point
  
  // The radiolarian that opens up
  lazy val radiolarian = 
    OBJSequence(
      path = "obj/OBJ_the_radiolarian_normale", 
      active = false, 
      stopAtEnd = true, 
      coreTransform = Transform(pos = Vec(3,3,3)),
      transform = Transform(pos = startPos, rot = Vec.random*360),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random))
    
  // The other radiolaria
  lazy val quasiradiolarians = 
    Array(
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_I.obj").toModel(
        transform = Transform(pos = startPos, size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white),
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_V.obj").toModel(
        transform = Transform(pos = startPos + Vec(-5,14,12), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
        coreTransform = Transform(pos = Vec(3,3,3)),
        color = white), //majhen
      OBJModel("obj/Ogromni_modeli/Plascek_normale_IV.obj").toModel(
        transform = Transform(pos = startPos + Vec(5,5,5), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
        coreTransform = Transform(pos = Vec(3,3,3)),
        color = white), //majhen
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VI.obj").toModel(
        transform = Transform(pos = startPos + Vec(5,5,5), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white),
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_VII.obj").toModel(
        transform = Transform(pos = startPos + Vec(5,5,5), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white),
      OBJModel("obj/Ogromni_modeli/Plascek_normale_IX_velik.obj").toModel(
        transform = Transform(pos = startPos + Vec(5,5,5), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
        coreTransform = Transform(pos = Vec(3,3,3)),
        color = white), //test it
      OBJModel("obj/Plascki_iz_stene/Plascek_normale_IX_mali.obj").toModel(
        transform = Transform(pos = startPos + Vec(5,5,5), size = Vec1),
        transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
        coreTransform = Transform(pos = Vec(5,5,5)),
        color = white)
  )

  // The rock inside radiolarians
  lazy val core = OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(color = Color(0.225,0.225,0.225))

  // Some rocks just floating around
  lazy val rocks = Array(
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_I.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(4,4,4)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),///////////////////////////////////////////////////////////////////duplication
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_I.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(4,4,4)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = startPos + Vec(5,5,5), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random11),
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

  /// CircleSpace phase objects ///
  def newStar = OBJModel("obj/UV_sfera/UV_sfera_I.obj").toModel(
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

  // have a small bump for movement
  var shakeBump = 3
  def shakeBumpN = 50


  // Oculus rift head tracking
  var lastRotation = Rotation(0,0,0)
  var rotation = Rotation(0,0,0)
  
  var frames = 0L
  def renderFrame() {
    frames += 1

    // Generate/Get oscilators, and heart signals
    val harhar = 20
    var beat = (frames % 70) == 0
    val heart = (frames % 70) match {
      case 0 => 1
      case x if x < harhar => pow(0.6, x)
      case x if x > harhar => pow(0.75, 70-x)
      case _ => 0
    }

    // http://www.dspguru.com/dsp/howtos/how-to-create-oscillators-in-software
    // f = desired oscillator frequency
    // w = 2 pi f / Fs 
    // phi = initial phase
    // y = ouput array containing the oscillator signal
    //def oscillator(i: Double) = sin((2*Pi*f)*i + phi)

    def oscillator(i: Double, phi: Double) = sin(i + phi)

    val osc1 = sin(Utils.now*0.002 + 0)
    val osc2 = sin(Utils.now*0.002 + 1*Pi/4)
    val osc3 = sin(Utils.now*0.002 + 2*Pi/4)
    val osc4 = sin(Utils.now*0.002 + 3*Pi/4)

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

    /*
      linux -> arduino

      prehod med fazami
      setup -> heartbeat works, rift view is forward, images preloaded ->
      radiolaridans -> radiolarian comes close, opens, black screen ->
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
    
    def glClear(g: Double) {
      GL11.glClearColor(g.toFloat,g.toFloat,g.toFloat,1)
      GL11.glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    }
    def drawFrontCam() {
      val img = stereoCameras(1).getTextureID
      val camScale = -1030
      val camhCorrect = 650
      val (camw, camh) = (winHeight*16/9d, winHeight+camhCorrect)
      val (camx, camy) = (winWidth/2-camw/2, -camhCorrect/2)
      quad(Coord(camx,camy,camw,camh)+camScale, img, alpha = 1, flipy = true, flipx = true)
    }

    if(fade < 1) fade += fadeSpeed else fade = 1

    phase match {
      case Setup => /////////////////////////////////////////////////////////////////////////////////////////////
        glClear(0)

        val (camw, camh) = (winHeight*4/3d, winHeight)
        val (camx, camy) = (winWidth/2-camw/2, 0)
        quad(Coord(winWidth/2-940/2,winHeight/2-550/2,940,550), liminoidTitle, alpha = 1)

        // Triggers the lazy compute or preload sequence
        println("Time" + (frames-1) + ": " + Utils.time((frames-1) match {
          case 0 => 
          case 1 => 
          case 2 => radiolarian
          case 3 => quasiradiolarians
          case 4 => rocks
          case 5 => //blackMandala.preload(24); whiteMandala.preload(24)
          case 6 => //blackHeartMandala; blackHeartDustMandala; whiteHeartMandala
          case 7 => //wall
          case 8 => //magnets
          case _ => gotoPhase(Radiolarians)
        }))
        
        System.gc()
        System.gc() // JVM, just do it, please

      case -1 => /////////////////////////////////////////////////////////////////////////////////////////////
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
        glClear(0)
        drawFrontCam()

        radiolarian.transform.setPosZ(radiolarian.transform.pos.z + testNum*5)
        testNum = 0
        radiolarian.transform.setPosX(radiolarian.transform.pos.x + testNum2*5)
        testNum2 = 0
        radiolarian.transform.setPosY(radiolarian.transform.pos.y + testNum3*5)
        testNum3 = 0

        // Render Camera
        Model.cam.lookAt(Vec3(0,0,500))
        Model.cam.render
        val ddd = 4 + (testNum+700)/100f
        Model.cam.rot = Vec3(-rotation.pitch/ddd, rotation.yaw/ddd, rotation.roll/ddd)

        val izSteneTimeProper = 17
        val izSteneTimeProper = 35
        val radioVectorTimeProper = 60+25
        val radioOpenTimeProper = 60+43
        val endTimeProper = 120+8
        
        val izSteneTime = 0
        var izStene = since(phaseTimer) > izSteneTime
        val tresenjeTime = izSteneTime + 5000
        val tresenje = since(phaseTimer) > tresenjeTime
        val radioVectorTime = tresenjeTime + 5000
        val radioVector = since(phaseTimer) > radioVectorTime
        val radioOpenTime = radioVectorTime + 5000
        val radioOpen = since(phaseTimer) > radioOpenTime
        val endTime = radioOpenTime + 5000
        val end = since(phaseTimer) > endTime


        /// Fix these.
        // When radiolarian is close enough, change phase
        if(radiolarian.transform.pos.z < 1) { 
          gotoPhase(Mandalas)
        }
        // Pull radiolarian towards view
        if(radiolarian.transform.pos.z < 150) { 
          radiolarian.transformVector.rot *= 0.8
          val z = radiolarian.transformVector.pos.z * 1.01
          radiolarian.transformVector.pos = ((Vec(0,0,0) - radiolarian.transform.pos).normalize*4).setZ(z)
        }
        // Activate radiolarian shell open animation
        if(radiolarian.transform.pos.z < 120 && !radiolarian.active) { 
          radiolarian.active = true; 
          fade = 0
        }

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

          /*val transforms: Array[(Model.MutableTransform, Model.MutableTransform)] = 
            (Array((radiolarian.transform, radiolarian.transformVector)) ++
              quasiradiolarians.map { a => (a.transform, a.transformVector)} ++
              rocks.map { a => (a.transform, a.transformVector)})

          // Prevent them from getting away or bumping
          def fixTransform(t: Model.MutableTransform, tVector: Model.MutableTransform) {
            if(t.pos.z > wallZ && izStene)
              tVector.pos = tVector.pos.setZ(-(abs(tVector.pos.z))*1.01)

            if(t.pos.z < 20 && izStene)
              tVector.pos = tVector.pos.setZ(abs(tVector.pos.z)*0.90)

            if(abs(t.pos.y) >= 170 && izStene) 
              tVector.pos = tVector.pos.setY(-signum(tVector.pos.y)*abs(tVector.pos.y)*0.9)

            if(abs(t.pos.x) >= 170 && izStene) 
              tVector.pos = tVector.pos.setX(-signum(tVector.pos.x)*abs(tVector.pos.x)*0.9)
          }*/


          /// 1
          // Postavi objekte glede na pleksi
          // Pocasen napad glede na muziko
          // ob <t> nastavi vektor in kot proti uporabniku
          /// 2
          // Zoom
          /// 3 
          // Fade
          // Tweak

          // Debounce them
          /*for(i <- 0 until transforms.length-1) {
            for(j <- i+1 until transforms.length) {
              if(i != j && (transforms(i)._1.pos distance transforms(j)._1.pos) < 5) {
                if(i != 0) {
                  //transforms(i)._2.pos = transforms(i)._2.pos*0.8 + (transforms(i)._1.pos - transforms(j)._1.pos).normalize * transforms(i)._2.pos.avg * 0.2
                }
                transforms(j)._2.pos = transforms(j)._2.pos*0.8 + ((transforms(j)._1.pos - transforms(i)._1.pos) / 5) * 0.2
              }
            }
          }*/

          radiolarian.transformVector.pos = radiolarian.transformVector.pos + Vec.random11.setZ(0)/20
          for(radio <- quasiradiolarians) radio.transformVector.pos = radio.transformVector.pos + Vec.random11.setZ(0)/20
          for(rock <- rocks) rock.transformVector.pos = rock.transformVector.pos + Vec.random11.setZ(0)/20
          //val z = radiolarian.transformVector.pos.z
          //radiolarian.transformVector.pos = (radiolarian.transformVector.pos).setZ(z)

          // Draw radiolarians
          val oscDiv = 10
          if(!pause) radiolarian.transform += radiolarian.transformVector
          radiolarian.transform.size = Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv) * 1.2
          radiolarian().render() // duplication below

          // Make core go black after radiolarian opening
          if(radiolarian.active) core.color -= 0.002
          core.render(transform = radiolarian.transform.copy(size = radiolarian.transform.size * radiolarian.coreTransform.pos.x))

          for(radio <- quasiradiolarians) {
            //fixTransform(radio.transform, radio.transformVector)

            if(!pause) radio.transform += radio.transformVector
            radio.transform.size = Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv)
            radio.render()

            core.render(transform = radio.transform.copy(size = radio.transform.size * radio.coreTransform.pos.x))
            //core.render(transform = radio.transform.copy(size = radio.transform.size * 5))
          }

          for(rock <- rocks) {
            //fixTransform(rock.transform, rock.transformVector)

            if(!pause) rock.transform += rock.transformVector
            rock.render()
          }
        }

      case Mandalas => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade = 0
          Sound.play("mandalas")
        }

        val div = 1.75d
        val (w, h) = (640*0.85, 800*0.85)
        val posx = winWidth/2d-w/2d + rotx/div
        val posy = winHeight/2d-h/2d + roty/div //don't forget the duplication below...

        if(blackMandala.active) {
          glClear(0)
          quad(Coord(posx,posy, w, h), blackMandala(), alpha = 1)
          fade = 0
        } else if(whiteMandala.active) {
          if(whiteFlashTimer == -1) {
            whiteFlashTimer = now
            glClear(1)
          } else if(since(whiteFlashTimer) <= 30*1000) { //30s
            glClear(1-fade*75+heart)
            fade = heart
          } else {
            glClear(fade*75)
            beat = false
          }

          quad(Coord(posx,posy, w, h), whiteMandala(), alpha = 1)
          //if(fade < 1) quad(Coord(posx,posy, w, h), blackHeartDustMandala, alpha = 1-fade*2+heart/5)
        } else {
          gotoPhase(CircleSpace)
        }
        val sinceStart = since(phaseTimer)
        if((sinceStart > 105*1000 || !blackMandala.active) && sinceStart < 3*60*1000) {//1m45s...3m  heartbeat sound and visualization
          if(startDustHeart == -1) startDustHeart = now

          if(beat) Sound.play("heart")

          if(blackMandala.active) {
            if(since(startDustHeart) > 15*1000) {//15s
              val (ww, hh) = (w*0.8, w*0.8)
              val posx = winWidth/2d-ww/2d + rotx/div
              val posy = winHeight/2d-hh/2d + roty/div
              quad(Coord(posx,posy, ww, hh), blackHeartMandala, alpha = heart*0.7)
            }

            quad(Coord(posx,posy, w, h), blackHeartDustMandala, alpha = heart)
          } else if(whiteMandala.active) {
            //quad(Coord(posx,posy, w, h), whiteHeartMandala, alpha = heart)
          }
        }

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

        def getVec(phi: Double, theta: Double) = {
          Vec(
            x = (cos(theta)*cos(phi)) * radius,
            y = (cos(theta)*sin(phi)) * radius,
            z = (sin(theta)) * radius)
        }
        def getDiff(phi: Double, theta0: Double, theta1: Double) = {
          getVec(phi,theta1) - getVec(phi,theta0)
        }
        def zeroDist(v: Vec) = v distance Vec(0,0,0)

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
            /*particles ++= Vector.fill(100)(Particle(
              transform = star.transform.copy(),
              transformVector = star.transformVector.copy(pos = -star.transformVector.pos, rot = Vec.random),
              //fade = nextDouble*2,
              color = Color(0.01+nextDouble*0.3))
            )*/
          }
          star.render(color = Color(0,0,0))
          star.trail += star.transform.pos.copy()
          star.trail.render()
        }

        /*particles = particles.filterNot(_.dead).map { p => 
          val zd = zeroDist(p.transform.pos);
          p.transform.pos = p.transform.pos * (radius/zd)
          p
        }
        particles.foreach { _.render }*/


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


        //core.render(transform = Model.Transform001)


      case BackSpace => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade = 0
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

      case _ =>
    }

    //quad(Coord(0,0,1280,800), rift, mode = G.Normal())
  }
  
  def processInput() {
    import Keyboard._

    if(isKeyDown(KEY_X)) Sound.play("jump")
    if(isKeyDown(KEY_1)) testNum -= 1
    if(isKeyDown(KEY_2)) testNum += 1
    if(isKeyDown(KEY_3)) testNum2 -= 1
    if(isKeyDown(KEY_4)) testNum2 += 1
    if(isKeyDown(KEY_PERIOD)) testNum3 -= 1
    if(isKeyDown(KEY_COMMA))  testNum3 += 1
    
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
    if(isKeyDown(KEY_UP)) modelSeq.pos = modelSeq.pos.copy(z = modelSeq.pos.z + 1)
    if(isKeyDown(KEY_DOWN)) modelSeq.pos = modelSeq.pos.copy(z = modelSeq.pos.z - 1)
    if(isKeyDown(KEY_LEFT)) modelSeq.pos = modelSeq.pos.copy(x = modelSeq.pos.x + 1)
    if(isKeyDown(KEY_RIGHT)) modelSeq.pos = modelSeq.pos.copy(x = modelSeq.pos.x - 1)

    if(isKeyDown(KEY_W)) modelSeq.rot = modelSeq.rot.copy(z = modelSeq.rot.z + 2)
    if(isKeyDown(KEY_S)) modelSeq.rot = modelSeq.rot.copy(z = modelSeq.rot.z - 2)
    if(isKeyDown(KEY_A)) modelSeq.rot = modelSeq.rot.copy(y = modelSeq.rot.y + 2)
    if(isKeyDown(KEY_D)) modelSeq.rot = modelSeq.rot.copy(y = modelSeq.rot.y - 2)
    if(isKeyDown(KEY_Q)) modelSeq.rot = modelSeq.rot.copy(x = modelSeq.rot.x + 2)
    if(isKeyDown(KEY_E)) modelSeq.rot = modelSeq.rot.copy(x = modelSeq.rot.x -s 2)
    */

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
      backCamera.getTextureIDWait;
      backCamera.saveImage("img/Image.png");
      backPixels = Vector.empty;
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
