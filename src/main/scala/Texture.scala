package org.ljudmila.liminoid

import org.lwjgl.opengl.GL30
import org.lwjgl.opengl.GL12
import org.lwjgl.opengl.GL11._
import java.io._
import java.nio._
import de.matthiasmann.twl.utils.PNGDecoder
import scala.collection.mutable

object Texture {
  val cache = mutable.HashMap[String, Int]()

  def apply(filename: String, mode: Int = GL_NEAREST_MIPMAP_LINEAR): Int = cache.getOrElseUpdate(filename,
    try {
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
      val buf = ByteBuffer.allocateDirect(c * w * h)
      decoder.decode(buf, w * c, PNGDecoder.Format.RGBA)
      buf.flip()

      in.close()
  
      val textureID = glGenTextures //Generate texture ID
      glBindTexture(GL_TEXTURE_2D, textureID) //Bind texture ID
      
      //Setup wrap mode
      //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
      //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)

      //Setup texture scaling filtering
      //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
      //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, mode)
      //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mode)
      
      //Send texel data to OpenGL
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      //gluBuild2DMipmaps(GL_TEXTURE_2D, c, w, h, GL_RGBA, GL_UNSIGNED_BYTE, buf);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, buf)
      GL30.glGenerateMipmap(GL_TEXTURE_2D);

      //Return the texture ID so we can bind it later again
      //println(textureID)

      textureID
    } catch {
      case e: Exception => 
        println(filename)
        e.printStackTrace
        -1
    })
}
