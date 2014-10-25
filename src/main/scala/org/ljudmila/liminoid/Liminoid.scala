package org.ljudmila.liminoid

import org.lwjgl.opengl.{ Display, PixelFormat, DisplayMode, Util }
import org.lwjgl.input.{ Keyboard, Mouse }
import collection.mutable
import collection.parallel.mutable.ParArray
import java.nio._
import scala.actors.Futures._
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

import Models.{ Transform, OBJModel, OBJModels, quad, Coord, Point, Model, Pixel }
import Models.{ Color, grey, grey0, grey1 }
import Models.{ Vec, vec, vecx, vecy, vecz, vec0, vec05, vec1, vec2, vec3, vec4, vec5, vec90x }
import Models.{ Rotation, rotation0 }
import Models.{ Thread, ThreadNetwork, ThreadNode, Line, RenderProcessData, RenderRenderData }

final object Liminoid {
  val project = "Liminoid"
  val settings = SettingsReader.load("Settings.txt")
  val dataFolder = settings("dataFolder")
  var startLiminoid = false

  var renderMode: RenderMode = RenderMode(settings("renderMode"))

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
    Sound.init(dataFolder + "snd/")

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
    // TODO try antialiasing/multisampling/...:
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
        if(currentPhase == Radiolarians) println("radiolarian: "+radiolarian.transform)
        println("rotation:"+rotation)
        println("Particles: "+backPixels.size)

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
    // TODO clear color and depth buffer?
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
    if(phaseChanged || currentPhase >= PhaseTerminator) return
    currentPhase += 1
    phaseChanged = true
  }
  def previousPhase(): Unit = {
    if(phaseChanged || currentPhase <= Setup) return
    currentPhase -= 1
    phaseChanged = true
  }
  def initPhase(initFunc: => Unit): Unit = 
    if(phaseChanged) {
      phaseTimer = now
      phaseChanged = false
      initFunc
      System.gc() // best place to do it...
    }

  // http://www.dspguru.com/dsp/howtos/how-to-create-oscillators-in-software
  // f = desired oscillator frequency
  // w = 2 pi f / Fs
  // phi = initial phase
  // y = ouput array containing the oscillator signal
  // def oscillator(i: Double) = sin((2*Pi*f)*i + phi)
  def oscillator(i: Double = Utils.now*0.002, phi: Double): Double = sin(i + phi)
  var osc1 = 0d
  var osc2 = 0d
  var osc3 = 0d
  var osc4 = 0d

  // variables that go up to 1 by little each frame
  var (fade1, fadeSpeed1) = (1d, 0.002)
  var (fade2, fadeSpeed2) = (1d, 0.04)
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
  
  val frontCamera = hardware.Camera(camId = settings("frontCameraId").toInt, settings("frontCameraX").toInt, settings("frontCameraY").toInt)
  val backCamera  = hardware.Camera(camId = settings("backCameraId").toInt, settings("backCameraX").toInt, settings("backCameraY").toInt)
  
  val flipFrontCamera = true//false

  val eyeCorrection = -64 // Eye shift for 2D rift view
  var testNum1, testNum2, testNum3 = 0 // Numbers for testing

  val winCoord = Coord(0,0, winWidth,winHeight)
  val coord2000 = Coord(0,0, 2000,2000)
  /// Radiolarians phase objects ///
  val whiteish = grey(0.9)
  val blackish = grey(0.1)

  val wallZ = settings("wallz").toInt // z position of wall
  val radioBasePosVecZ = -0.11 * 1.4 // 2min8s -> 1min30s (85x + 43x*1.75 = ...)
  val radioBasePosVec = Vec(0, 0, radioBasePosVecZ) // basic z movement vector
  val startPos = vecz(wallZ+15) // starting point for radiolarians
  def basicRot(): Vec = Vec.randomGaussian(1d/3d)
  
  // The rock inside radiolarians
  lazy val core = 
    OBJModel.load(settings("coreModel")).toModel(color = Color(0.05, 0.05, 0.05))

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
  lazy val rocks = 
    (OBJModels(
        settings("rocks1"),
        Transform(pos = startPos),
        Transform(pos = radioBasePosVec, rot = basicRot)) ++
    OBJModels(
        settings("rocks2"),
        Transform(pos = startPos),
        Transform(pos = radioBasePosVec, rot = basicRot)) ++
    OBJModels(
        settings("rocks3"),
        Transform(pos = startPos),
        Transform(pos = radioBasePosVec, rot = basicRot)))

  /// Mandalas phase objects ///
  val startFrameNum = 2162 // filenames for mandalas start with this
  lazy val blackMandala = 
    TexSequence(
      settings("blackMandala"),
      delay = 24.FPS,
      selfDestruct = true)
  lazy val whiteMandala = 
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
  var back0Seq = 
    TexSequence(
      settings("back0"),
      delay = 1000/15d,
      stopAtEnd = false,
      bounce = true,
      snap = Texture.getImage(settings("back0")+"1.png"))
  var back1Seq = 
    TexSequence(
      settings("back1"),
      delay = 1000/15d,
      stopAtEnd = false,
      bounce = true,
      snap = Texture.getImage(settings("back1")+"100.png"))
  var back2Seq = 
    TexSequence(
      settings("back2"),
      delay = 1000/15d,
      stopAtEnd = false,
      bounce = true,
      snap = Texture.getImage(settings("back2")+"100.png"))
  var back3Seq = 
    TexSequence(
      settings("back3"),
      delay = 1000/15d,
      stopAtEnd = false,
      bounce = true,
      snap = Texture.getImage(settings("back3")+"100.png"))
  //var backCamSnapTex = backCamera.getTextureIDWait
  var backPixels = Vector.empty[Pixel]
  var backpixelBuffer = Array[Array[Boolean]]()
  var backPixelDrop = false
  var backPixelMerge = false
  var backPixelMerged = true
  var finished = false
  var diffStarted, diffDone = false

  lazy val threadNetwork = ThreadNetwork(settings("threadNetwork"))

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

  def drawFrontCam(): Unit = {
    val img = frontCamera.getTextureID
    val camScale = -1030
    val camhCorrect = 650
    val (camw, camh) = ((winHeight*(16d/9d)).toInt, winHeight+camhCorrect)
    val (camx, camy) = (winWidth/2-camw/2, -camhCorrect/2)
    quad(Coord(camx,camy, camw,camh)+camScale, img, flipy = flipFrontCamera, flipx = true)
  }
  
  var schizoidPhase = 1

  val backBlend = (GL_ONE, GL_ONE)
  val backBlendColor = Color(0, 0.08, 0.18)
  def backBlendRender(): Unit = {
    /*GL14.glBlendEquation(GL14.GL_FUNC_REVERSE_SUBTRACT)
    val alpha = if(currentPhase == Radiolarians) 0.07 else 0.1
    quad(winCoord, color = backBlendColor, alpha = alpha, blend = backBlend)
    GL14.glBlendEquation(GL14.GL_FUNC_ADD)*/
  }

  var softHeart = 0d
  var softHeart2 = 0d

  var frames = 0L
  def renderFrame(): Unit = {
    frames += 1

    // Generate/Get heart signals
    if(!PulseSensor.init_) {
      println("Using fake pulse")
      PulseSensor.fake = true
      PulseSensor.init_ = true
    }
    var beat = PulseSensor.takeBeat
    val heart = if(beat) 1d else 0d
    softHeart = (heart + softHeart)*0.8
    softHeart2 = (softHeart + softHeart2)*0.7

    osc1 = oscillator(phi = 0*Pi/4)
    osc2 = oscillator(phi = 1*Pi/4)
    osc3 = oscillator(phi = 2*Pi/4)
    osc4 = oscillator(phi = 3*Pi/4)

    if(fade1 < 1) fade1 += fadeSpeed1 else fade1 = 1
    if(fade2 < 1) fade2 += fadeSpeed2 else fade2 = 1
    if(fadeFlash < 1) fadeFlash += fadeSpeedFlash else fadeFlash = 1
    if(fadeSlow < 1) fadeSlow += fadeSpeedSlow else fadeSlow = 1

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


    currentPhase match {
      
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Setup => ///////////////////////////////////////////////////////////////////////////////
        glClear1d(0)

        quad(Coord(winWidth/2-940/2,winHeight/2-550/2, 940,550), if(frames <= 10 && !startLiminoid) liminoidSplashLoading else liminoidSplash)

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
        var startingPhase = phases(settings("startingPhase"))
        //startingPhase = Mandalas
        //startingPhase = Schizoid
        //gotoPhase(Threads)

        // Triggers lazy load or preload of some resources
        println("Time" + frames + ": " + Utils.time(
          frames match {
            case 1 => 
            case 2 => if(startingPhase <= Radiolarians || startingPhase == FlyingRock) radiolarian
            case 3 => if(startingPhase <= Radiolarians || startingPhase == FlyingRock) rocks
            case 4 => 
            case 5 => if(startingPhase <= Mandalas) blackMandala.preload(300)
            case 6 => if(startingPhase <= Mandalas) blackHeartMandala
            case 7 => if(startingPhase <= Mandalas) blackHeartDustMandala
            case 8 => if(startingPhase <= Mandalas) whiteHeartMandala
            case 9 => if(startingPhase <= Mandalas) whiteMandala.preload(200)
            case _ => if(startLiminoid) gotoPhase(startingPhase) else sleep(25)
          }))
        
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
        }

        /// Parts of the Radiolarians phase
        val izSteneStart     = 17.seconds
        val tresenjeStart    = 35.seconds
        val radioVectorStart = (60+25-38).seconds
        val radioOpenStart   = (60+43-38).seconds
        val endStart         = (120+8-38).seconds

        // Crawl from wall
        val izStene = since(phaseTimer) > izSteneStart
        val firstIzStene = (!prevIzStene && izStene)

        // Start shaking elements
        val tresenje = since(phaseTimer) > tresenjeStart
        val firstTresenje = (!prevTresenje && tresenje)

        // Pull radiolarian towards view
        val radioVector = since(phaseTimer) > radioVectorStart
        val firstRadioVector = (!prevRadioVector && radioVector)

        // Activate radiolarian shell open animation
        val radioOpen = since(phaseTimer) > radioOpenStart
        val firstRadioOpen = (!prevRadioOpen && radioOpen)

        if(firstRadioOpen) radiolarian.active = true
        val radioHalfOpen = radioOpen && radiolarian.cursor > radiolarian.frames.size/2

        // Start of radiolarians phase
        val end = since(phaseTimer) > endStart
        val firstEnd = (!prevEnd && end)

        if((radiolarian.transform.pos distance vec0) < 1 
        || (radiolarian.transform.pos.z < -1)) {
          gotoPhase(Mandalas)
        }


        /// Rendering
        glClear1d(0)
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
          // Draw invisible wall
          glCapability(GL_DEPTH_TEST, GL_BLEND) {
            glTheUsualBlendFunc
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
          if(firstTresenje) {
            fade1 = 0
          }
          def shake(m: Model): Transform = {
            val newPos =
              if(tresenje) {
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
          if(!pause) radiolarian.transform += radiolarian.transformVector ** renderTime
          if(firstRadioVector) {
            radiolarian.transformVector.pos = radiolarian.transformVector.pos.withZ(radioBasePosVecZ*1.75)
          }
          
          if(firstRadioOpen) {
            fade1 = 0
          }
          
          radiolarian.transform.size = {
            val scale = 1.7
            
            (if(radioOpen) {
              (radiolarianSize * (scale * (1 - fade1))) + vec((scale + 1/oscDiv) * fade1)
            } else {
              radiolarianSize * scale
            })
          }


          val shaked: Transform = if(radioOpen) radiolarian.transform else shake(radiolarian())
          radiolarian().render(transform = shaked) // duplication below
          
          // Make core go black after radiolarian opening
          if(radioHalfOpen) core.color -= 0.0002
          
          core.render(transform = shaked.copy(size = radiolarian.transform.size * radiolarian.coreTransform.pos.x))
          
          // Draw rocks
          glMatrix {
            for(rock <- rocks) {
              if(!pause) rock.transform += rock.transformVector ** renderTime
              if(radioVector) {
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

        backBlendRender


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
            (640*0.85 + (if(blackMandala.active) 0 else heartBeating), 
             800*0.85 + (if(blackMandala.active) 0 else heartBeating))
        val posx = winWidth/2-w/2d + rotx/div
        val posy = winHeight/2-h/2d + roty/div //don't forget the duplication below...

        if(blackMandala.cursor < 2000) {
          fadeSlow = 0;
        }
        glClear1d(if(blackMandala.active) 0 else 1)
        if (blackMandala.active) {
          quad(Coord(posx,posy, w,h), blackMandala())
          if(fade2 < 1) quad(coord2000, alpha = 1 - fade2, color = grey0)

          fade1 = 0
        } else if (whiteMandala.active) {
          quad(Coord(posx,posy, w,h), whiteMandala())
        } else if(!blackMandala.active && !whiteMandala.active) {
          gotoPhase(Threads)
        }

        if(blackMandala.cursor > 5001-startFrameNum) {
          if(startDustHeart == -1) startDustHeart = now

          if(beat) {
            Sound.play(if(blackMandala.active) "heartbeep" else "heartbeat")
          }
          
          if(blackMandala.active) {
            if(since(startDustHeart) > 10.seconds) {
              val (ww, hh) = (w*0.8, h*0.8)
              val posx = winWidth/2-ww/2d + rotx/div
              val posy = winHeight/2-hh/2d + roty/div
              quad(Coord(posx,posy, ww,hh), blackHeartMandala, alpha = softHeart2*0.6)
            }
            
            quad(Coord(posx,posy, w,h), blackHeartDustMandala, alpha = softHeart)
          }
        }

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
      /////////////////////////////////////////////////////////////////////////////////////////////
      case Schizoid => ////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade1 = 0
          fade2 = 1
          diffStarted = false
          diffDone = false
          
          backPixels = Vector.empty
          backPixelDrop = true
          for(i <- 1 to 5) backCamera.getTextureIDWait
          Sound.play("razpad")
        }
        
        glClear1d(0)
        
        val f = 1400d
        val (camresx, camresy) = (backCamera.width, backCamera.height)
        val camaspect = camresx/camresy.toDouble
        val (camw, camh) = (f*camaspect, f) //(winHeight*4/3d, winHeight)
        val (camx, camy) = (rotx*0.7-camw/7, roty*0.7-camh/7)
        
        val camSchizoidPhases = Set[Int]()
        
        lazy val (backCFrame, backCSnap) = backCamera.getTextureIDandSnap()
        lazy val (back0Frame, back0Snap) = (back0Seq(), back0Seq.snap)
        lazy val (back1Frame, back1Snap) = (back1Seq(), back1Seq.snap)
        lazy val (back2Frame, back2Snap) = (back2Seq(), back2Seq.snap)
        
        val phasesSnap =
          Array(
            backCSnap, backCSnap,
            back0Snap, back1Snap,
            back0Snap, back2Snap,
            back0Snap, backCSnap,
            back0Snap, back1Snap,
            back0Snap, back2Snap,
            back0Snap, backCSnap)
        val phases =
          Array(
            backCFrame, backCFrame,
            back0Frame, back1Frame,
            back0Frame, back2Frame,
            back0Frame, backCFrame,
            back0Frame, back1Frame,
            back0Frame, back2Frame,
            back0Frame, backCFrame)

        quad(Coord(camx,camy, camw,camh), phases(schizoidPhase), flipx = false)
        if (fade2 < 1) {
          quad(Coord(camx,camy, camw,camh), phases(schizoidPhase-1), flipx = false, alpha = 1-fade2);
        }
        
        def resetStuff() {
          phaseTimer = now
          backPixelDrop = true
          finished = false
          
          diffDone = false
          diffStarted = false
        }
        if (Keyboard.isKeyDown(Keyboard.KEY_I)) {
          resetStuff();
        }
        def sincen(i: Int) = since(phaseTimer) >= i.seconds
        
        val break1 = 5
        val break2 = break1 + 5
        val break3 = break2 + 5
        val break4 = break3 + 5

        if(sincen(break1) && backPixelDrop && !finished) {
          if(!diffStarted) thread {
            diffDone = false
            diffStarted = true
            backPixels = backCamera.getDiffBlob(phasesSnap(schizoidPhase+1), phasesSnap(schizoidPhase), settings("threshold").toInt, 1280, 720)
            val center = {
              var x, y = 0d
              for(bp <- backPixels) {
                x += bp.x
                y += bp.y
              }
              
              Vec(x/backPixels.size, y/backPixels.size, 0)
            }
            
            for(bp <- backPixels) {
              bp.transformVector = Vec(
                  (bp.x - center.x)/7d + TableRandom.nextGaussian/0.7d,
                  (bp.y - center.y)/40d + TableRandom.nextGaussian/0.7d,
                  0)
            }
            Sound.play("razpadheart1")
            diffDone = true
          } else if(diffDone) {
            diffStarted = false
            diffDone = false
            backPixelDrop = false
            backPixelMerged = false
            backPixelMerge = true;
            for(bp <- backPixels) {
              bp.newColor = Color.BGR(phasesSnap(schizoidPhase+2)((bp.sx + bp.sy*1280).toInt))
            }
            fade2 = 0;
            schizoidPhase += 2
            //phaseTimer = now
          }
        } /*else if(sincen(break2)) {
          for(bp <- backPixels) {
            //bp.transformVector = bp.transformVector * 0.995 //Vec((bp.sx - bp.x)/200, (bp.sy - bp.y)/200, 0)
            /*if(((bp.original && nextDouble < 0.0075) || nextDouble < 0.0035) && backPixels.size < 123456) {
              backPixels :+= makePix(bp.sx, bp.sy, 150);
            }
            if(nextDouble < 0.008 && sincen(5) && bp.original) bp.isDead = true*/
          }
        }*/ else if(sincen(break2) && backPixelMerge) {
          //for(bp <- backPixels) {
            
            //bp.transformVector = Vec((bp.sx - bp.x)/300, (bp.sy - bp.y)/300, 0)
            //bp.color = Color.BGR(phasesSeq(schizoidPhase+1).snap((bp.sx*camratiow + (bp.sy*camratioh)*1280).toInt))
            //bp.newColor = Color.BGR(phasesSnap(schizoidPhase)((bp.sx + bp.sy*1280).toInt))
            //bp.isDead = true
            
          //}
          backPixelMerge = false
          backPixelMerged = true
        } else if(sincen(break2+3) && backPixelMerged) {  
          //fade2 = 0;
          backPixelMerged = false
        }
        
        if(sincen(break3)) {
          resetStuff();
        }
        
        
        /*else if(since5 && !backPixelDrop && !finished) {
          backPixelDrop = true
          finished = true
          phaseTimer = now
          Sound.play("razpadheart2")
          backPixelMerge = true
        } else if(since5 && backPixelMerge) {
          for(bp <- backPixels) {
            bp.transformVector = bp.transformVector * 0.8 //Vec((bp.sx - bp.x)/200, (bp.sy - bp.y)/200, 0)
            if(nextDouble < 0.01) backPixels :+= makePix(bp.sx, bp.sy);
          }
          backPixelMerged = false
          backPixelMerge = false
          phaseTimer = now
        } else if(since1 && !backPixelMerged && finished) {
          Sound.play("razpadheart1")
          backPixelMerged = true
          backPixelDrop = false
          thread { sleep(300); backPixels = Vector.empty }
        }*/
        
        glDisable(GL_BLEND)
        render2D {
          glTranslated(camx+testNum1, camy+testNum2, 0)
          glScaled(camw/camresx, camh/camresy, 1)
          
          //glDisable(GL_POINT_SMOOTH); glPrimitive(GL_POINTS) {
          glPrimitive(GL_QUADS) {
            // Filter and render pixels at the same time
            if(!backPixelDrop) {
              backPixels = backPixels.filterNot { bp =>
                bp.transformVector = bp.transformVector*0.90
                bp.x = bp.x*0.93 + bp.sx*0.07
                bp.y = bp.y*0.93 + bp.sy*0.07
                bp.render();
                bp.isDead
               }
            }
          }
        }
        //glEnable(GL_BLEND)
        glColor4d(1,1,1,1)

        if(fade1 < 1) {
          //quad(coord2000, alpha = 1-fade1)
        }
        
        /*implicit val rpd = RenderProcessData(beat)
        implicit val rrd = RenderRenderData(camx, camy, camw, camh)
        threadNetwork.process
        threadNetwork.render*/

        //backBlendRender


      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      //case Threads => /////////////////////////////////////////////////////////////////////////////
      /*  initPhase {
          //fade1 = 0
          //fade2 = 0

          //backCamera.saveImage(dataFolder + "img/Image.png")
          backPixels = Vector.empty
          //backCamSnap = Texture.getImage(dataFolder + "img/Image.png")
          //backCamSnapTex = Texture(dataFolder + "img/Image.png")
        }

        val f = 1400d
        val (camw, camh) = (f*16/9d, f) //(winHeight*4/3d, winHeight)
        val (camx, camy) = (rotx*0.7-camw/7, roty*0.7-camh/7)

        glClear1d(0)
        quad(Coord(camx,camy, camw,camh), backCamera.getTextureID, flipx = false)
*/
        // Pass some data into thread network renders
        


        //val since = since(phaseTimer)  >= 5.seconds
        

        //backBlendRender
        
        /*if (testNum1 != 0) {
          var _i = 0
          def i() = { _i += 1; _i - 1 } 
          if (flash == i) {
        	  println("flash tiem!")
        	  fadeFlash = 0
            flash = 1
          } else if (flash == i) {
            quad(coord2000, alpha = fadeFlash)
            if(fadeFlash == 1) {
          	  fadeFlash = 0
              flash += 1
            }
          } else if (flash == i) {
            quad(coord2000, alpha = 1)
            if(fadeFlash == 1) {
          	  fadeFlash = 0
              flash += 1
            }
          } else if (flash == i) {
            if(fadeFlash == 1) flash += 1
            quad(coord2000, alpha = 1-fadeFlash)
            if(fadeFlash == 1) {
          	  fadeFlash = 0
              flash += 1
            }
          } else if (flash == i) {
            nextPhase
            flash += 1
            /*core.color = whiteish
            core.transform.pos = startPos
            core.transform.size = radiolarian.transform.size * radiolarian.coreTransform.pos.x
            core.transformVector.pos = radioBasePosVec
            core.transformVector.rot = basicRot*/
          } else if (flash == i) {
            //if(!pause) core.transform += core.transformVector ** renderTime
            //core.render(transform = core.transform)
            testNum1 = 0
            flash = 0
          }
        }*/
        
        backBlendRender
        
        

      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      case FlyingRock => //////////////////////////////////////////////////////////////////////////
        initPhase {
          core.color = whiteish
          core.transform.pos = startPos
          core.transform.size = radiolarian.transform.size * radiolarian.coreTransform.pos.x
          core.transformVector.pos = radioBasePosVec
          core.transformVector.rot = basicRot
        }
        
        val f = 1400d
        val (camw, camh) = (f*16/9d, f) //(winHeight*4/3d, winHeight)
        val (camx, camy) = (rotx*0.7-camw/7, roty*0.7-camh/7)

        glClear1d(0)
        quad(Coord(camx,camy, camw,camh), backCamera.getTextureID, flipx = false)

        if(!pause) core.transform += core.transformVector ** renderTime
        core.render(transform = core.transform)
        
        backBlendRender
        
                
      case _ =>
    }
  }
  
  var flash = 0;
  
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
    
    if(isKeyDown(KEY_B)) {
      for(i <- 1 to 1000) {
        makePix(winWidth/3, winHeight/3);
      }
    }
    

    if(isKeyDown(KEY_NEXT)) nextPhase
    if(isKeyDown(KEY_PRIOR)) previousPhase
    if(isKeyDown(KEY_HOME)) gotoPhase(Radiolarians)
    if(isKeyDown(KEY_END)) gotoPhase(PhaseTerminator)
    if(isKeyDown(KEY_P)) { pause = !pause; sleep(200) }

    if(isKeyDown(KEY_X)) {
      // Takes about 5 frames to set exposure, let's wait...
      for(i <- 1 to 10) backCamera.getTextureIDWait
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
    if(isKeyDown(KEY_Z)) {
      println("Making new Back...")
      Sound.play("razpadheart1")
      // Takes about 5 frames to set exposure, let's wait 20...
      for(i <- 1 to 20) backCamera.getTextureIDWait
      val n = 100
      for(i <- 1 to n) {
        backCamera.saveImage(settings("backN") + i + ".png")
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
    if(isKeyDown(KEY_C)) { 
      renderMode = (if(renderMode == Mono) Stereo else Mono); sleep(200) 
    }
    if(isKeyDown(KEY_M)) { 
      Sound.mute() 
    }

    if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
      isMainLoopRunning = false
    }
  }
  
  def makePix(sx: Double, sy: Double, max: Int = 256) = {
    val pix = Pixel(sx = sx, sy = sy, color = Color.Gray(nextInt(max)))
    pix.transformVector = Vec(TableRandom.nextGaussian*2, TableRandom.nextGaussian*2, 0)
    pix.original = false
    pix
  }
  
}
