package org.ljudmila.liminoid.hardware

import com.googlecode.javacv._
import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_highgui._
import collection.mutable
import scala.actors.Futures._
import System.err
import org.ljudmila.liminoid.Model

object Camera {
  val FrameGrabbers = mutable.HashMap[Int, OpenCVFrameGrabber]()
  val supressErrors = true
  
  def getCamera(camIds: Seq[Int], width: Int = 640, height: Int = 480): Option[Camera] = {
    for(camId <- camIds) {
      val out = Camera(camId, width, height)
      if(out.isStarted) return Some(out)
    }
    
    None
  }
  
  def getFrameGrabber(camId: Int = 0, width: Int = 640, height: Int = 480): Option[OpenCVFrameGrabber] = {
    try {
      val cam = new OpenCVFrameGrabber(camId) 
      cam.setImageWidth(width)
      cam.setImageHeight(width)
      cam.start
      cam.grab
      FrameGrabbers += camId -> cam
      Some(cam)
    } catch {
      case e: Exception => 
      err.println("Failed to initialize camera "+camId+" @ "+width+"x"+height)
      None
    }
  }
  def apply(camId: Int = 0, width: Int = 640, height: Int = 480): Camera = {
    new Camera(camId, width, height)
  }
}
class Camera(val camId: Int = 0, val width: Int = 640, val height: Int = 480) {
  private var camOpt: Option[OpenCVFrameGrabber] = Camera.getFrameGrabber(camId, width, height)
  def isStarted: Boolean = camOpt.isDefined
  private lazy val cam = camOpt.get

  import org.lwjgl.opengl.GL11._
  import org.lwjgl.opengl.GL12._
  private def captureFrameImg(): IplImage = cam.grab
  var lastTextureID = -1
  private def captureFrameTex(img: IplImage): Int = {
    if(img == null) return lastTextureID // fixes ocassional dropped frame
    
    //Generate texture and bind ID
    val textureID = glGenTextures 
    glBindTexture(GL_TEXTURE_2D, textureID)
    
    //Setup wrap mode
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    
    //Setup texture scaling filtering
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    
    //Send texel data to OpenGL
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, img.width,img.height, 0, GL_BGR, GL_UNSIGNED_BYTE, img.getByteBuffer)
    
    //Return the texture ID so we can bind it later again
    //println(textureID)
    lastTextureID = textureID
    textureID
  }

  def saveImage(filename: String): Unit = { 
    try {
      cvSaveImage(filename, captureFrameImg())
    } catch {
      case e: Exception =>
        println(filename)
        throw e
    }
  }

  def getDiffBlob(pixels1: Array[Int]): Vector[Model.Pixel] = {
    val img = captureFrameImg()
    if(img == null) return Vector.empty
    val image = img.getBufferedImage
    val (w, h) = (image.getWidth, image.getHeight)
    val size = w*h
    val pixels2 = Array.ofDim[Int](size)

    image.getRGB(0,0, w,h, pixels2, 0,w)

    val threshold = 70
    def compare(c1: Int, c2: Int): Int = math.abs(
      ((c1 & 255) - (c2 & 255)) //+
      //(((c1 >> 8) & 255) - ((c2 >> 8) & 255)) +
      //(((c1 >> 16) & 255) - ((c2 >> 16) & 255))
    ) /// 3

    var pix = Vector.empty[Model.Pixel]
    for(i <- 0 until size by 2) if((i/w)%2 == 0 && i%w > 1 && i%w < w-1 && i > w && i < size-w && compare(pixels1(i), pixels2(i)) > threshold) 
      pix :+= Model.Pixel(x = i%w, y = i/w, color = Model.Color.BGR(pixels2(i)))

    pix
  }

  //
  private var camtexFuture = future[IplImage] { null }
  private var camTex = -1
  def getTextureID(): Int = synchronized { try {
    if(camTex == -1) {
      camTex = captureFrameTex(captureFrameImg())
      camtexFuture = future { captureFrameImg() }
    } else if(camtexFuture.isSet) {
      glDeleteTextures(camTex)
      camTex = captureFrameTex(camtexFuture())
      camtexFuture = future { captureFrameImg() }
    }
    camTex
  } catch { case x: Exception => if(Camera.supressErrors) -1 else throw x } }

  def getTextureIDWait(): Int = synchronized { try {
    var limit = 100
    var tex = captureFrameTex(captureFrameImg())
    while(tex == -1 || tex == camTex) {
      tex = captureFrameTex(captureFrameImg())
      limit -= 1
      Thread.sleep(20)
    }

    tex
  } catch { case x: Exception => if(Camera.supressErrors) -1 else throw x } }
}
