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

import Model.{Transform, Vec, Vec0, Vec1, OBJModel, Color, Particle}

final object Liminoid {
  val project = "Liminoid"

  sealed trait RenderMode
  case class Normal() extends RenderMode
  case class Split() extends RenderMode
  val renderMode: RenderMode = Normal()

  var isMainLoopRunning = false
  var renderTime = 0f
  var lastFPS = 0f
  var pause = false

  // Cameras
  //supressErrors...
  val cams = Array(hardware.Camera(camId = 0, 1920, 1080), hardware.Camera(camId = 1), hardware.Camera(camId = 2), hardware.Camera(camId = 3))
  val backCamera = cams.head
  val stereoCameras = cams.take(2)
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
  
  // Liminoid phases
  val Setup = 0
  val Radiolarians = 1
  val Mandalas = 2
  val CircleSpace = 3
  val BackSpace = 4
  val PhaseTerminator = BackSpace
  var phaseTimer = now

  var phase = Setup // Set initial phase
  var phaseChange = true
  
  def gotoPhase(i: Int) {
    phase = i
    phaseChange = true
  }
  def nextPhase() {
    if(phaseChange || phase >= PhaseTerminator) return
    phase += 1
    phaseChange = true
  }
  def previousPhase() {
    if(phaseChange || phase <= Setup) return
    phase -= 1
    phaseChange = true
  }
  def initPhase(f: => Unit): Unit = if(phaseChange) {
    phaseTimer = now
    phaseChange = false
    f
    System.gc() //best place to do it...
  }

  val eyeCorrection = -64
  var testNum = 0;

  // Radiolarians phase objects
  lazy val white = Color(0.85)
  val wallZ = 200 // z position of wall
  var radioBasePosVec = Vec(0,0,-0.1)
  lazy val radiolarians = {
    val radiolarians = Array.fill(4)(OBJSequence("obj/Radiolarian", active = false, stopAtEnd = true, transformVector = Transform(pos = radioBasePosVec, rot = Vec.random)))
    radiolarians(0).transform += Transform(pos = Vec(0,0,wallZ+15),    rot = Vec(90,0,0))
    radiolarians(1).transform += Transform(pos = Vec(30,8,wallZ+24),   rot = Vec(120,11,33))
    radiolarians(2).transform += Transform(pos = Vec(-53,13,wallZ+92), rot = Vec(223,45,143))
    radiolarians(3).transform += Transform(pos = Vec(84,-31,wallZ+48), rot = Vec(321,92,234))

    radiolarians
  }
  lazy val quasiradiolarians = Array(
    OBJModel("obj/Prihod_iz_stene/Plascek_normale_I.obj").toModel(
      transform = Transform(pos = Vec(-42,25,wallZ+15), rot = Vec(221,24,32), size = Vec(1,1,1)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Plascek_normale_I.obj").toModel(
      transform = Transform(pos = Vec(22,15,wallZ+100), rot = Vec(221,24,32), size = Vec(1,1,1)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random),
      color = white)
  )
  lazy val core = OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_I.obj").toModel(color = Color(0.25,0.25,0.25))

  lazy val rocks = Array(
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_I.obj").toModel(
      transform = Transform(pos = Vec(40,14,wallZ+16), rot = Vec(120,71,77), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_II.obj").toModel(
      transform = Transform(pos = Vec(-32,-4,wallZ+28), rot = Vec(144,11,13), size = Vec(3,3,3)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_III.obj").toModel(
      transform = Transform(pos = Vec(77,-22,wallZ+67), rot = Vec(112,43,95), size = Vec(4,4,4)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_IV.obj").toModel(
      transform = Transform(pos = Vec(-92,15,wallZ+75), rot = Vec(231,28,42), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random),
      color = white),
    OBJModel("obj/Prihod_iz_stene/Prihod iz stene_normale_V.obj").toModel(
      transform = Transform(pos = Vec(42,55,wallZ+55), rot = Vec(221,24,32), size = Vec(2,2,2)),
      transformVector = Transform(pos = radioBasePosVec, rot = Vec.random),
      color = white)
  )
  var particles = Vector.empty[Particle]

  // Mandalas phase objects
  val blackMandala = new TexSequence("seq/optipng_Sekvenca_mandala_crno_ozadje", delay = 1000/24d, stopAtEnd = true, selfDestruct = true)
  val whiteMandala = new TexSequence("seq/optipng_Sekvenca_mandala_belo_ozadje", delay = 1000/24d, stopAtEnd = true, selfDestruct = true)
  var whiteFlashTimer = -1
  var startHeart = -1
  var startDustHeart = -1
  var lastHeartpos = 0d
  lazy val blackHeartMandala = Texture("seq/Srcni_utrip_CO/Srcni_utrip_CO_01290.png")
  lazy val blackHeartDustMandala = Texture("seq/Srcni_utrip_CO_II/Srcni_utrip_CO_II_01287.png")
  lazy val whiteHeartMandala = Texture("seq/Srcni_utrip_BO/Srcni_utrip_BO_05848.png")

  // CircleSpace phase objects
  def newStar = OBJModel("obj/UV_sfera/UV_sfera_I.obj").toModel(
    transform = Transform(rot = Vec.random, size = (Vec1/2) + (Vec.random/3)),
    transformVector = Transform(rot = Vec.random),
    color =  Color(0.2,0.2,0.2),
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
  lazy val wall = Texture("img/Image.png")

  var fade = 1d

  var lastRotation = Rotation(0,0,0)
  var rotation = Rotation(0,0,0)
  
  var frames = 0L
  def renderFrame() {
    frames += 1

    // Generate/Get oscilators, and heart signals
    val harhar = 20
    val beat = (frames % 70) == 0
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
      
    //G.quad(G.Coord(rotx + cx-frames*ratio/2d, roty + cy-frames/2d,mw+frames*ratio,mh+frames), (seqs.head)(), alpha = 1-abs(osc1)/4)
    //G.quad(G.Coord(0,0,1920,1080) + osc1*30, num(), alpha = 1)
    
    def glClear(g: Double) {
      GL11.glClearColor(g.toFloat,g.toFloat,g.toFloat,1)
      GL11.glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    }

    if(fade < 1) fade += 0.002 else fade = 1

    phase match {
      case Setup => /////////////////////////////////////////////////////////////////////////////////////////////
        glClear(0)

        val (camw, camh) = (winHeight*4/3d, winHeight)
        val (camx, camy) = (winWidth/2-camw/2, 0)
        G.quad(G.Coord(winWidth/2-940/2,winHeight/2-550/2,940,550), liminoidTitle, alpha = 1)
        //G.quad(G.Coord(camx,camy,camw,camh), backCamera.getTextureID, alpha = 1)

        // preload - 
        println(Utils.time(frames match {
          case 1 => //Foo
          case 2 => for(radio <- radiolarians) { radio.preload }
          case 3 => quasiradiolarians
          case 4 => rocks // just triggers the lazy compute
          case 5 => blackMandala.preload(100); whiteMandala.preload(100)
          case 6 => blackHeartMandala; blackHeartDustMandala; whiteHeartMandala
          case 7 => wall
          case 8 => magnets
          case 9 => gotoPhase(Mandalas)
        }))

      case Radiolarians => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          Sound.play("intro")
        }
        glClear(1)

        // When radiolarian is close enough, change phase
        if(radiolarians.exists { _.transform.pos.z < 0 }) { gotoPhase(Mandalas); Sound.stopAll() }
        // Activate radiolarian shell open animation
        radiolarians.find { _.transform.pos.z < 100 }.map { r => r.transformVector.rot *= 0.8 }
        radiolarians.find { _.transform.pos.z < 50 }.map { r => if(!r.active) { r.active = true; fade = 0 } }

        val (camw, camh) = (winHeight*4/3d, winHeight)
        val (camx, camy) = (winWidth/2-camw/2, 0)
        stereoCameras(0).getTextureID
        stereoCameras(1).getTextureID
        
        G.quad(G.Coord(camx,camy,camw,camh) + testNum, backCamera.getTextureID, alpha = 1)

        // Render Camera
        Model.cam.render

        if(since(phaseTimer) > 5*1000) {
          // Draw invisible wall
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

          // Draw radiolarians
          val oscDiv = 10
          for(radio <- radiolarians) {
            radio.transform += radio.transformVector
            radio.transform.size = Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv)
            //radio.transform.rot += Vec(osc1,osc2,osc3)
            radio().render()

            // Make core go blac
            if(radio.active) core.color -= 0.0015
            
            core.render(transform = radio.transform.copy(size = radio.transform.size * 5))
          }
          for(radio <- quasiradiolarians) {
            radio.transform += radio.transformVector
            radio.transform.size = Vec(1+osc1/oscDiv, 1+osc2/oscDiv, 1+osc3/oscDiv)
            //radio.transform.rot += Vec(osc1,osc2,osc3)
            radio.render()

            core.render(transform = radio.transform.copy(size = radio.transform.size * 5))
          }

          for(rock <- rocks) {
            rock.transform += rock.transformVector
            rock.render()
          }
        }

      case Mandalas => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade = 0
          //Sound.mute
          Sound.play("mandalas")
        }

        if(blackMandala.active) {
          glClear(0)
          G.quad(G.Coord(winWidth/2-640/2,winHeight/2-800/2,640,800), blackMandala(), alpha = 1)
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
          }

          G.quad(G.Coord(winWidth/2-640/2,winHeight/2-800/2,640,800), whiteMandala(), alpha = 1)
          if(fade < 1) G.quad(G.Coord(winWidth/2-640/2,winHeight/2-800/2,640,800), blackHeartDustMandala, alpha = 1-fade*2+heart/5)
        } else {
          gotoPhase(CircleSpace)
        }
        val sinceStart = since(phaseTimer)
        if((sinceStart > 105*1000 || !blackMandala.active) && sinceStart < 3*60*1000) {//1m45s...3m  heartbeat sound and visualization
          if(startDustHeart == -1) startDustHeart = now

          if(beat) Sound.play("heart")

          if(blackMandala.active) {
            if(since(startDustHeart) > 15*1000) {//15s
              val w = 640*0.8
              val h = 800*0.8
              G.quad(G.Coord(winWidth/2-w/2,winHeight/2-h/2,w,h), blackHeartMandala, alpha = heart*0.7)
            }

            G.quad(G.Coord(winWidth/2-640/2,winHeight/2-800/2,640,800), blackHeartDustMandala, alpha = heart)
          } else if(whiteMandala.active) {
            G.quad(G.Coord(winWidth/2-640/2,winHeight/2-800/2,640,800), whiteHeartMandala, alpha = heart)
          }
        }

      case CircleSpace => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade = 0
          frames = 0
          stars = Vector.fill(30)(newStar)
        }
        val radius = 100
        glClear(1)

        //Model.cam.lookAt(Vec3(testNum,0,0))
        Model.cam.lookAt(Vec3(testNum,-20,1))
        Model.cam.render

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
            val ratio = if(magdist < magthresh) math.pow((magthresh-magdist)/magthresh, 300) else 1
            star.transform.pos += diff*ratio + magnet.transform.pos*(1 - ratio)
            star.transformVector.pos = diff
            if(magdist < magnet.transform.size.avg && nextDouble < 0.01) {
              star.transform.pos = Vec.random
              magnet.transform.size += Vec.randomUniform
            }
            
            val zeroD = zeroDist(star.transform.pos)
            if(abs(zeroD - radius) > 0) {
              star.transform.pos = star.transform.pos * (radius/zeroD)
            }
            star.theta = theta1
            star.phi += nextDouble*0.005
            particles ++= Vector.fill(100)(Particle(
              transform = star.transform.copy(),
              transformVector = star.transformVector.copy(pos = -star.transformVector.pos, rot = Vec.random),
              //fade = nextDouble*2,
              color = Color(0.01+nextDouble*0.3))
            )
          }

          star.render(color = Color(0,0,0))
        }

        particles = particles.filterNot(_.dead).map { p => 
          val zd = zeroDist(p.transform.pos);
          p.transform.pos = p.transform.pos * (radius/zd)
          p
        }
        particles.foreach { _.render }

        //stars.head.render(color = Color(0.5, 0.5, 0.5), alpha = 0.25, transform = Transform(pos = Vec(0,0,0), size = Vec(radius,radius,radius)))
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


        //core.render(transform = Model.Transform001)


      case BackSpace => /////////////////////////////////////////////////////////////////////////////////////////////
        initPhase {
          fade = 0
        }
        
        glClear(1)

        val (camw, camh) = (winHeight*4/3d, winHeight)
        val (camx, camy) = (winWidth/2-camw/2, 0)
        G.quad(G.Coord(camx,camy,camw,camh), backCamera.getTextureID, alpha = 1, flipx = true)
        G.quad(G.Coord(camx,camy,camw,camh), wall, alpha = heart, flipx = true)
      case _ =>
    }

    //G.quad(G.Coord(0,0,1280,800), rift, mode = G.Normal())
  }
  
  def processInput() {
    import Keyboard._

    if(isKeyDown(KEY_X)) Sound.play("jump")
    if(isKeyDown(KEY_1)) testNum -= 1
    if(isKeyDown(KEY_2)) testNum += 1
    
    if(isKeyDown(KEY_W)) Model.cam.pos.z += 1
    if(isKeyDown(KEY_S)) Model.cam.pos.z -= 1
    if(isKeyDown(KEY_D)) Model.cam.pos.x -= 1
    if(isKeyDown(KEY_A)) Model.cam.pos.x += 1
    if(isKeyDown(KEY_Q)) Model.cam.pos.y -= 1
    if(isKeyDown(KEY_E)) Model.cam.pos.y += 1

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
    if(isKeyDown(KEY_P)) pause = !pause
    if(isKeyDown(KEY_0)) Model.cam.pos = Vec3(0,0,0)

    if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
      isMainLoopRunning = false
    }    
  }
}
