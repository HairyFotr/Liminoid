package org.ljudmila.liminoid.hardware

import com.googlecode.javacv._
import com.googlecode.javacv.cpp.opencv_core._
import collection.mutable.{HashMap,HashSet,ListBuffer,LinkedHashMap}
import System.err


object Camera {
  val FrameGrabbers = HashMap[Int, OpenCVFrameGrabber]()
  
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
  var camOpt: Option[OpenCVFrameGrabber] = Camera.getFrameGrabber(camId, width, height)
  def isStarted: Boolean = camOpt.isDefined
  lazy val cam = camOpt.get
  lazy val grabRange = (0 until width*height)
  
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

  val pixels = Array.ofDim[Int](this.width * this.height)
  def getColor(p:(Double,Double)):(Double,Double,Double) = {
    val (x,y) = (p._1.toInt, p._2.toInt)
    //val pixel = pixels((this.width*this.height - 1) -  (y * this.width + x))
    val pixel = pixels((y * this.width + x))
    
    (
      ((pixel >> 16) & 0xFF).toByte/255d,
      ((pixel >> 8) & 0xFF).toByte/255d,
      (pixel & 0xFF).toByte/255d
    )
  }
  import org.lwjgl.BufferUtils
  import org.lwjgl.opengl.GL11._
  import org.lwjgl.opengl.GL12
  def captureFrameImg(): IplImage = cam.grab
  def captureFrameTex(img: IplImage): Int = {
    if(img == null) return -1
    val image = img.getBufferedImage
    val (w, h) = (image.getWidth, image.getHeight)
    val size = w*h
    
    image.getRGB(0, 0, w, h, pixels, 0, w)

    val buffer = BufferUtils.createByteBuffer(size * 3) //4 for RGBA, 3 for RGB
    
    for(y <- 0 until h; x <- 0 until w) {
      val pixel = pixels((size - 1) -  (y * w + x))
      //buffer.put(((pixel >> 24) & 0xFF).toByte)    // Alpha component. Only for RGBA
      buffer.put(((pixel >> 16) & 0xFF).toByte)     // Red component
      buffer.put(((pixel >> 8) & 0xFF).toByte)      // Green component
      buffer.put((pixel & 0xFF).toByte)           // Blue component
    }
    
    buffer.flip //FOR THE LOVE OF GOD DO NOT FORGET THIS

    // You now have a ByteBuffer filled with the color data of each pixel.
    // Now just create a texture ID and bind it. Then you can load it using 
    // whatever OpenGL method you want, for example:

    val textureID = glGenTextures //Generate texture ID
    glBindTexture(GL_TEXTURE_2D, textureID) //Bind texture ID
    
    //Setup wrap mode
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL12.GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL12.GL_CLAMP_TO_EDGE)

    //Setup texture scaling filtering
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    
    //Send texel data to OpenGL
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, buffer)

    //Return the texture ID so we can bind it later again
    //println(textureID)
    textureID
  }
}  
