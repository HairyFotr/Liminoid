package org.ljudmila.liminoid.hardware

import com.googlecode.javacv._
import com.googlecode.javacv.cpp.opencv_core._
import collection.mutable.{HashMap,HashSet,ListBuffer,LinkedHashMap}
import scala.actors.Futures._
import System.err


object Camera {
  val FrameGrabbers = HashMap[Int, OpenCVFrameGrabber]()
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
  private lazy val grabRange = (0 until width*height).toArray
  
  def captureFrame(pixels: Array[Array[Int]]) {
    for(img <- Option(cam.grab)) {
      val imgData = img.getBufferedImage.getData.getDataBuffer.asInstanceOf[java.awt.image.DataBufferByte] //surely there's an easier way
      for(i <- grabRange) {
        pixels(i%width)(i/width) = (
          imgData.getElem(i*3 + 0).toInt +
          imgData.getElem(i*3 + 1).toInt +
          imgData.getElem(i*3 + 2).toInt)
      }
    }
  }

  import org.lwjgl.opengl.GL11._
  import org.lwjgl.opengl.GL12._
  private def captureFrameImg(): IplImage = cam.grab
  private def captureFrameTex(img: IplImage): Int = {
    if(img == null) return -1
    
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
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, img.width, img.height, 0, GL_BGR, GL_UNSIGNED_BYTE, img.getByteBuffer)
    
    //Return the texture ID so we can bind it later again
    //println(textureID)
    textureID
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
}
