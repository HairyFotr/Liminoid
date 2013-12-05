package org.ljudmila.liminoid

import org.lwjgl.opengl.{Display,PixelFormat,DisplayMode,Util}
import org.lwjgl.input.Keyboard
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

import OBJModel.{Transform, Vec, Vec0, Vec1, Vec2}
// 


final object Liminoid {
  val project = "Liminoid"

  sealed trait RenderMode
  case class Normal() extends RenderMode
  case class Split() extends RenderMode
  val renderMode: RenderMode = Normal()

  var isMainLoopRunning = false
  var renderTime = 0f
  var lastFPS = 0f

  // Cameras
  val cam = hardware.Camera(camId = 0)
  //val camBack = hardware.Camera(camId = 1)

  /**
   * Initializes display and enters main loop
   */
  def main(args: Array[String]) {
    initDisplay()
    Sound.init()
    RiftTracker.init()

    mainLoop()
    
    // Cleanup
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
        case Split() =>
          //shader.beginOffScreenRenderPass();
          renderFrame()  // draw stuff
          //shader.endOffScreenRenderPass();
          //shader.renderToScreen();

          /*
          //shader.beginOffScreenRenderPass();
          Particles += 
            Particle(
              Quad(
                coord = Vec3(nextFloat*100-50, nextFloat*100-50, nextFloat*100-50),
                size = Size(nextFloat*1, nextFloat*1)), 
              (quad: Quad, delta: Float) => {
                quad.coord += Vec3(nextFloat-nextFloat, nextFloat-nextFloat, nextFloat-nextFloat)
                quad
              })
          Particles.render(1);
          //shader.endOffScreenRenderPass();
          //shader.renderToScreen();
          //shader.beginOffScreenRenderPass();
          Particles += 
            Particle(
              Quad(
                coord = Vec3(nextFloat*100-50, nextFloat*100-50, nextFloat*100-50),
                size = Size(nextFloat*1, nextFloat*1)), 
              (quad: Quad, delta: Float) => {
                quad.coord += Vec3(nextFloat-nextFloat, nextFloat-nextFloat, nextFloat-nextFloat)
                quad
              })
          Particles.render(2);
          //shader.endOffScreenRenderPass();
          //shader.renderToScreen();
          */
      }
      
      Display.update() // update window contents and process input messages
      frameCounter += 1

      if(currentTime-FPStimer > second*FPSseconds) {
        val FPS = frameCounter/FPSseconds.toFloat

        println("FPS: "+FPS)
        println("testNum: "+testNum)
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
     
    //camView.setPerspective(50, winWidth/winHeight.toFloat, 1f, 600f)
    //camView.setOrtho(0,0,1,1,-100f,100f)
    //camView.setRotation(0,0,0)
    //camView.setPosition(0,0,-1)
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
  //val startZoom = -200
  var camTex = -1
  var camtexFuture = future { cam.captureFrameImg() }
  
  // Liminoid phases
  val Setup = 0
  val Radiolarians = 1
  val Mandalas = 2
  val CircleSpace = 3
  val BackSpace = 4

  var phase = Setup
  
  val eyeCorrection = -64
  var testNum = 0;

  // Radiolarians phase objects
  lazy val room = Texture("img/wall.png")
  lazy val radiolarians = {
    val radiolarians = Array(
      OBJSequence("obj/Radiolarian", active = false, stopAtEnd = true),
      OBJSequence("obj/Radiolarian", active = false, stopAtEnd = true)
    )
    radiolarians(0).transforms = Transform(pos = Vec(0,0,200), rot = Vec(90,0,0))
    radiolarians(1).transforms = Transform(pos = Vec(20,8,210), rot = Vec(120,11,33))

    radiolarians
  }
  var radioPosVec = Vec(0,0,-0.5)
  var radioRotVec = Vec(0,0,0)
  lazy val core = OBJModel("obj/Prihod iz stene/Prihod iz stene_I_catclark.obj")

  lazy val rock1 = OBJModel("obj/Prihod iz stene/Prihod iz stene_II_catclark.obj")
  val rockTransform1 = Transform(pos = Vec(40,14,180), rot = Vec(120,11,33), size = Vec(2,2,2))
  lazy val rock2 = OBJModel("obj/Prihod iz stene/Prihod iz stene_III_catclark.obj")
  val rockTransform2 = Transform(pos = Vec(-32,-4,220), rot = Vec(120,11,33), size = Vec(3,3,3))

  // Mandalas phase objects
  val mainMandala = new TexSequence("seq/00/", delay = 1, stopAtEnd = true)

  // CircleSpace phase objects
  lazy val sphereTex = OBJModel("obj/UV_sfera/UV_sfera_I.obj")
  //val Particles
  

  var lastRotation = Rotation(0,0,0)
  var rotation = Rotation(0,0,0)
  
  var frames = 0L
  def renderFrame() {
    frames += 1

    // Generate/Get oscilators, and heart signals
    val heart = (frames % 70) match {
      case 0 => 1
      case x @ (1|2|3)    => pow(0.5, x)
      case x @ (69|68|67) => pow(0.75, 70-x)
      case _ => 0
    }

    // http://www.dspguru.com/dsp/howtos/how-to-create-oscillators-in-software
    // f = desired oscillator frequency
    // w = 2 pi f / Fs 
    // phi = initial phase
    // y = ouput array containing the oscillator signal
    // def oscillator(i: Double) = sin((2*Pi*f)*i + phi)

    val osc1 = sin(Utils.now*0.002 + 0)
    val osc2 = sin(Utils.now*0.002 + Pi/4)
    val osc3 = sin(Utils.now*0.002 + Pi/2)

    // Get camera frame
    if(camtexFuture.isSet) {
      glDeleteTextures(camTex)
      camTex = cam.captureFrameTex(camtexFuture())
      camtexFuture = future { cam.captureFrameImg() }
    }

    Util.checkGLError();
    
    val (mw, mh) = (200-osc1*100-osc3*30, 160-osc2*50-osc3*20)
    val (cx, cy) = (winWidth/2 - mw/2, winHeight/2 - mh/2)
    val ratio = mw/mh.toFloat

    // Get head rotation
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

      optimizations
        preload images while fps >= 60

      realne koordinate stene
      senca na steni
      floaterji so bolj na mestu
      radiolarians gredo na pogled proti tebi
      radiolarian s kuglo

      mandale
      mora bit posyncano
      pulz opacity layers

      to white, mogoce ze prej vletavajo utrini "asteroid field effect"

      utriniki po kroznici gredo skozi ~5 tock + trail (2 kota sin cos?)

      kugla z uv mapo kamere, upocasnitev pogleda

      OS support
        opencv je zaeenkrat treba rocno... obstaja sbt string?
        lib rift sem sam scompilal da sploh ne zaznava Rifta
        PNGDecoder pride z lwjgl oz. kje je sbt string?
      
    */
      
    //G.quad(G.Coord(rotx + cx-frames*ratio/2d, roty + cy-frames/2d,mw+frames*ratio,mh+frames), (seqs.head)(), alpha = 1-abs(osc1)/4)
    //G.quad(G.Coord(0,0,1920,1080) + osc1*30, num(), alpha = 1)
    
    def glClear(r: Float, g: Float, b: Float) {
      GL11.glClearColor(r,g,b,1)
      GL11.glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    }

    phase match {
      case Setup => 
        phase = Radiolarians

        // Preload everything
        for(radio <- radiolarians) {
          radio.rewind
          radio.active = true
          while(radio.active) radio().displayList
          radio.rewind
          radio.active = false
        }

        mainMandala.rewind
        mainMandala.active = true
        while(mainMandala.active) mainMandala()
        mainMandala.rewind
        mainMandala.active = true

        //TODO
        val (camw, camh) = (winHeight*4/3d, winHeight)
        val (camx, camy) = (winWidth/2-camw/2, 0)
        G.quad(G.Coord(camx,camy,camw,camh) + testNum, room, alpha = 1, flipx = true)
        //G.quad(G.Coord(camx,camy,camw,camh) + testNum, camTex, alpha = osc1, flipy = true, flipx = false)

        System.gc()

      case Radiolarians =>
        glClear(1,1,1)

        // When radiolarian is close enough, change phase
        if(radiolarians.exists { _.transforms.pos.z < -18 }) phase = Mandalas
        // Activate radiolarian shell open animation
        radiolarians.find { _.transforms.pos.z < 10 }.map { _.active = true }

        val (camw, camh) = (winHeight*4/3d, winHeight)
        val (camx, camy) = (winWidth/2-camw/2, 0)
        G.quad(G.Coord(camx,camy,camw,camh) + testNum, room, alpha = 1, flipx = true)
        //G.quad(G.Coord(camx,camy,camw,camh) + testNum, camTex, alpha = osc1, flipy = true, flipx = false)

        // Render Camera
        OBJModel.cam.render

        // Draw invisible wall
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glColor4f(1,1,1,0)
        glBegin(GL_QUADS)
          glVertex3d(-100, -100, 170)
          glVertex3d(+100, -100, 170)
          glVertex3d(+100, +100, 170)
          glVertex3d(-100, +100, 170)
        glEnd()
        glDisable(GL_DEPTH_TEST)
        glDisable(GL_BLEND)

        // Draw radiolarians
        val oscDiv = 15
        for(radio <- radiolarians) {
          radio.transforms.size = Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv)
          radio.transforms.pos += radioPosVec
          radio.transforms.rot += Vec(osc1,osc2,osc3)
          radio().render(t = radio.transforms)
          
          core.render(t = radio.transforms.copy(size = radio.transforms.size * 5), c = Vec0)
        }

        rock1.render(t = rockTransform1, c = Vec1)
        rockTransform1.pos += radioPosVec
        rock2.render(t = rockTransform2, c = Vec1)
        rockTransform2.pos += radioPosVec

      case Mandalas =>
        glClear(0,0,0)
        
        G.quad(G.Coord(0,0,winWidth,winHeight), mainMandala(), alpha = 0.5+(1-heart)*0.5)
        if(!mainMandala.active) phase = CircleSpace

      case CircleSpace =>
        glClear(1,1,1)

        phase = BackSpace
        //sphereTex.render(p = radio.pos, s = radio.size * 5, c = OBJModel.Vec0)

      case BackSpace =>
        Thread.sleep(5000)
        glClear(1,1,1)

        //if(radiolarians.exists { _.transforms.pos.z < -18 }) phase += 1
        //radiolarians.find { _.transforms.pos.z < 0 }.map { _.active = true }

        val (camw, camh) = (winHeight*4/3d, winHeight)
        val (camx, camy) = (winWidth/2-camw/2, 0)
        G.quad(G.Coord(camx,camy,camw,camh) + testNum, room, alpha = 1, flipx = true)
        //G.quad(G.Coord(camx,camy,camw,camh) + testNum, camTex, alpha = osc1, flipy = true, flipx = false)
      case _ =>
    }

    //G.quad(G.Coord(0,0,1280,800), rift, mode = G.Normal())
  }
  
  def processInput() {
    import Keyboard._

    if(isKeyDown(KEY_X)) Sound.play("jump")
    if(isKeyDown(KEY_1)) testNum -= 1
    if(isKeyDown(KEY_2)) testNum += 1
    
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
    if(isKeyDown(KEY_E)) modelSeq.rot = modelSeq.rot.copy(x = modelSeq.rot.x - 2)

    if(isKeyDown(KEY_RETURN)) modelSeq.active = !modelSeq.active*/

    if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
      isMainLoopRunning = false
    }    
  }
}
