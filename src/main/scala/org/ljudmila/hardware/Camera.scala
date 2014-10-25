package org.ljudmila.hardware

//import org.bytedeco.javacv._
//import org.bytedeco.javacpp.opencv_core._
//import org.bytedeco.javacpp.opencv_highgui._
import com.googlecode.javacv._
import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_highgui._;

import collection.mutable
import scala.actors.Futures._
import System.err
import org.ljudmila.liminoid.Models.{ Pixel, Color }
import org.ljudmila.Utils.thread

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
      cam.setImageHeight(height)
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
  var prevTextureID = -1
  private def captureFrameTex(img: IplImage): Int = {
    if(img == null) return prevTextureID // fixes ocassional dropped frame
    
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
    prevTextureID = textureID
    textureID
  }

  def saveImage(filename: String): Unit = {
    try {
      val img = captureFrameImg()
      // FIXME issues with libpng versions
      //thread { cvSaveImage(filename, img) }
      thread {
        javax.imageio.ImageIO.write(
          img.getBufferedImage(),
          "png",
          new java.io.File(filename))
      }
      
    } catch {
      case e: Exception =>
        println(filename)
        throw e
    }
  }

  def getSnap(): Array[Int] = {
    return image2Snap(captureFrameImg())
  }
  def image2Snap(img: IplImage): Array[Int] = {
    val image = img.getBufferedImage
    val (w, h) = (image.getWidth, image.getHeight)
    val size = w*h
    val pixels = Array.ofDim[Int](size)

    image.getRGB(0,0, w,h, pixels, 0,w)
    pixels
  }
  
  def getDiffBlob(pixels1: Array[Int], threshold: Int): Vector[Pixel] = {
    val img = captureFrameImg()
    val image = img.getBufferedImage
    val (w, h) = (image.getWidth, image.getHeight)
    val size = w*h
    val pixels2 = Array.ofDim[Int](size)

    image.getRGB(0,0, w,h, pixels2, 0,w)

    getDiffBlob(pixels1, pixels2, threshold, w,h)
  }

  def getDiffBlob(pixels1: Array[Int], pixels2: Array[Int], threshold: Int, w: Int, h: Int): Vector[Pixel] = {
    val size = w*h
    
    /*def compare(c1: Int, c2: Int): Int = math.abs(
      ((c1 & 255) - (c2 & 255)) //+
      //(((c1 >> 8) & 255) - ((c2 >> 8) & 255)) +
      //(((c1 >> 16) & 255) - ((c2 >> 16) & 255))
    ) /// 3*/
    def compare(c1: Int, c2: Int): Int = math.abs(
      ((c1 & 255) - (c2 & 255)) +
      (((c1 >> 8) & 255) - ((c2 >> 8) & 255)) +
      (((c1 >> 16) & 255) - ((c2 >> 16) & 255))
    ) / 3

    val pix = Vector.newBuilder[Pixel]
    val margin = 5
    var i = 0 
    do {
      val idw = i/w
      val imw = i%w
      if(imw > 1+margin && imw < w-1-margin && i > w && i < size-w && compare(pixels1(i), pixels2(i)) > threshold) {
        pix += Pixel(sx = imw, sy = idw, color = Color.BGR(pixels2(i)))
      }
      
      i += 1
    } while(i < size)
    pix.result

    /*var pix = Vector.empty[Pixel]
    for(i <- 0 until size) if(i%w > 1 && i%w < w-1 && i > w && i < size-w && compare(pixels1(i), pixels2(i)) > threshold) {
      pix :+= Pixel(sx = i%w, sy = i/w, color = Color.BGR(pixels2(i)))
    }
    pix*/
  }

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

  private var camCapture: IplImage = null
  private var camSnap = Array[Int]()
  
  def getTextureIDandSnap(): (Int, Array[Int]) = synchronized { try {
    if(camTex == -1) {
      camCapture = captureFrameImg()
      camTex = captureFrameTex(camCapture)
      camtexFuture = future { captureFrameImg() }
      camSnap = image2Snap(camCapture) 
    } else if(camtexFuture.isSet) {
      glDeleteTextures(camTex)
      camCapture = camtexFuture()
      camTex = captureFrameTex(camCapture)
      camtexFuture = future { captureFrameImg() }
      camSnap = image2Snap(camCapture) 
    }
    
    (camTex, camSnap)
  } catch { case x: Exception => if(Camera.supressErrors) (-1, Array()) else throw x } }

  def getTextureIDWait(): Int = synchronized { try {
    var limit = 100
    var tex = captureFrameTex(captureFrameImg())
    while(tex == -1 || tex == camTex) {
      tex = captureFrameTex(captureFrameImg())
      limit -= 1
      Thread.sleep(10)
    }

    tex
  } catch { case x: Exception => if(Camera.supressErrors) -1 else throw x } }
}
