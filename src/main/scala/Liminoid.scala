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
    glClearColor(0.1f, 0.1f, 0.1f, 1f)

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
    def floatBuffer(a: Float*): FloatBuffer = {
      ByteBuffer
        .allocateDirect(a.length*4)
        .order(ByteOrder.nativeOrder)
        .asFloatBuffer
        .put(a.toArray)
        .flip
        .asInstanceOf[FloatBuffer]
    }

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
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
  }
  
  /**
  * Renders current frame
  */
  var frames = 0L
  val startZoom = -200
  var camTex = -1
  var camtexFuture = future { cam.captureFrameImg() }
  

  lazy val rift = Texture("img/rift.png")
  val mandala = new TexSequence("seq/Pointilisticna_mandala/", delay = 100)
  val mandalaT = new TexSequence("seq/Transparentna_mandala/", delay = 10)
  val num = new TexSequence("seq/TestNumbers/", delay = 500)
  val num2 = new TexSequence("seq/TestNumbers/", delay = 500, bounce = false)
  val seqs = List(new TexSequence("seq/000/", delay = 50, bounce = true))
  val eyeCorrection = -64
  var testNum = 0;
  lazy val room = Texture("img/room2.png")
  lazy val model = OBJModel("obj/landscape_test_I.obj")
  lazy val modelSeq = OBJSequence("obj/Radiolarian", active = false)
  
  var lastRotation = Rotation(0,0,0)
  var rotation = Rotation(0,0,0)
  
  def renderFrame() {
    Util.checkGLError();
    frames += 1

    val heart = (frames % 70) match {
      case 0 => 1
      case x @ (1|2|3)    => pow(0.5, x)
      case x @ (69|68|67) => pow(0.75, 70-x)
      case _ => 0
    }

    val osc1 = sin(Utils.now*0.002 + 0)
    val osc2 = sin(Utils.now*0.002 + Pi/4)
    val osc3 = sin(Utils.now*0.002 + Pi/2)
    val gray = 0.2f;//(heart/3).toFloat;//((1-abs(osc2))*0.1).toFloat
    glClearColor(gray,gray,gray,1f)

    if(camtexFuture.isSet) {
      glDeleteTextures(camTex)
      camTex = cam.captureFrameTex(camtexFuture())
      camtexFuture = future { cam.captureFrameImg() }
    }

    //G.quad(G.Coord(100,100,800,500), mandalaT(), alpha = 0.5+(1-heart)*0.5)
    //G.quad(G.Coord(800,100,800,800) + osc1*20, num())
    //G.quad(G.Coord(800,900,100,100) + osc1*30, num2())
    //G.quad(G.Coord(500,500,250,250) + osc2*50, camTex, alpha = abs(osc2), flip = true)
    
    val (camw, camh) = (winHeight*4/3d, winHeight)
    val (camx, camy) = (winWidth/2-camw/2, 0)
    G.quad(G.Coord(camx,camy,camw,camh) + testNum, room, alpha = 1, flipx = true)
    //G.quad(G.Coord(camx,camy,camw,camh) + testNum, camTex, alpha = osc1, flipy = true, flipx = false)
    //G.quad(G.Coord(100,300,500,300) + osc1*10 + osc2*15, mandalaT())
    //G.quad(G.Coord(300,700,500,300) + osc1*10 + osc2*15, mandalaT())
    //G.quad(G.Coord(400,750,500,300) + osc1*10 + osc2*15, mandalaT())
    //G.quad(G.Coord(600,700,500,300) + osc1*10 + osc2*15, mandalaT())
    Util.checkGLError();
    
    val (mw, mh) = (200-osc1*100-osc3*30, 160-osc2*50-osc3*20)
    val (cx, cy) = (winWidth/2 - mw/2, winHeight/2 - mh/2)
    val ratio = mw/mh.toFloat

    val (rotx, roty) = 
      if(lastRotation == Rotation(0,0,0)) {
        lastRotation = RiftTracker.poll
        
        (0f,0f) 
      } else {
        val rot = RiftTracker.poll
        val rotDelta = rot - lastRotation
        lastRotation = rot*0.1f + lastRotation*0.9f
        rotation = rotation + rotDelta
        
        (-rotation.yaw, -rotation.pitch)
      }
    

    /*
      linux -> arduino

      stena
      floaters so bolj na mestu
      radiolarians gredo na pogled proti tebi
      radiolarian s kuglo

      to black

      mandale
      pulz

      to white

      utriniki po kroznici gredo skozi ~5 tock + trail (2 kota sin cos?)

      kugla z uv mapo kamere, upocasnitev pogleda
    */
      
    //G.quad(G.Coord(rotx + cx-frames*ratio/2d, roty + cy-frames/2d,mw+frames*ratio,mh+frames), (seqs.head)(), alpha = 1-abs(osc1)/4)
    //G.quad(G.Coord(0,0,1920,1080) + osc1*30, num(), alpha = 1)

    //if(frames % 50 == 0) Sound.play("jump")
    Util.checkGLError();

    val oscDiv = 15
    modelSeq.size = OBJModel.Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv)
    modelSeq().render(p = modelSeq.pos, r = modelSeq.rot, s = modelSeq.size)

    //G.quad(G.Coord(0,0,1280,800), rift, mode = G.Normal())
  }
  
  def processInput() {
    import Keyboard._
    
    if(isKeyDown(KEY_X)) Sound.play("jump")
    if(isKeyDown(KEY_1)) testNum -= 1
    if(isKeyDown(KEY_2)) testNum += 1
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

    if(isKeyDown(KEY_RETURN)) modelSeq.active = !modelSeq.active

    if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
      isMainLoopRunning = false
      return
    }
    
  }
}
