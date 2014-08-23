package org.ljudmila.liminoid

import org.lwjgl.opengl.GL30
import org.lwjgl.opengl.GL12
import org.lwjgl.opengl.GL11._
import java.io._
import java.nio._
import javax.imageio.ImageIO
import de.matthiasmann.twl.utils.PNGDecoder
import scala.collection.mutable

object Texture {
  private[this] val cache = mutable.AnyRefMap[String, Int]()

  protected final class Buffer(val w: Int, val h: Int, val buffer: ByteBuffer)
  
  def getImage(filename: String): Array[Int] = try {
    val image = ImageIO.read(new File(filename))
    val (w, h) = (image.getWidth, image.getHeight)
    val size = w*h
    val pixels = Array.ofDim[Int](size)
    
    image.getRGB(0,0, w,h, pixels, 0,w)

    pixels
  } catch {
    case e: Exception =>
      println(filename)
      throw e
  }

  def loadBuffer(filename: String): Buffer = try {
    //TODO: could I avoid OOM if I reused buffer and marked synchronized (have another method for mass preload)
    // Open the PNG file as an InputStream
    val in = new FileInputStream(filename)
    // Link the PNG decoder to this stream
    val decoder = new PNGDecoder(in)
    
    // Color components (3 RGB, 4 RGBA)
    val c = 4
    
    // Get the width and height of the texture
    val w = decoder.getWidth()
    val h = decoder.getHeight()
    
    // Decode the PNG file in a ByteBuffer
    val buffer = ByteBuffer.allocateDirect(c * w * h) //allocate
    decoder.decode(buffer, w * c, PNGDecoder.Format.RGBA)
    buffer.flip()

    in.close()

    new Buffer(w, h, buffer)
  } catch {
    case e: Exception =>
      println(filename)
      throw e
  }
  
  def makeTexture(buffer: Buffer): Int = {
    val textureID = glGenTextures //Generate texture ID
    glBindTexture(GL_TEXTURE_2D, textureID) //Bind texture ID
    
    //Setup wrap mode
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)

    //Setup texture scaling filtering
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mode)
    
    // MipMap
    /*glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR)
    //Send texel data to OpenGL
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, buffer.w,buffer.h, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer.buffer)
    GL30.glGenerateMipmap(GL_TEXTURE_2D)*/

    // Linear
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    //Send texel data to OpenGL
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, buffer.w,buffer.h, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer.buffer)

    buffer.buffer.clear()

    textureID
  }
  
  def apply(filename: String): Int = cache.getOrElseUpdate(filename, makeTexture(loadBuffer(filename)))

  def preload(files: Array[File], max: Int = -1): Unit = {
    try {
      (if(max == -1) files else files.take(max))
        .filterNot { file => cache.contains(file.toString) }
        .par.map { file => (file.toString, loadBuffer(file.toString)) }
        .seq.foreach { case (filename, buffer) => cache(filename) = makeTexture(buffer) }
    } catch {
      case t: Throwable =>
        t.printStackTrace
    }
  }

  def delete(a: String): Unit = {
    if(cache contains a) {
      glDeleteTextures(cache(a))
      cache -= a
    }
  }
}
