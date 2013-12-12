package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._

/// find focus points, render with glscissor

object G {
  import Liminoid.{winWidth, winHeight, renderMode, RenderMode, Normal, Split, eyeCorrection}
  private val cam = new Camera
  cam.setViewPort(0,0,winWidth,winHeight)
  cam.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  
  case class Coord(x: Double, y: Double, w: Double, h: Double) {
    def +(d: Double): Coord = Coord(x - d/2, y - d/2, w + d, h + d)
  }

  def quad(coord: Coord, texture: Int = -1, flipx: Boolean = false, flipy: Boolean = false, alpha: Double = 1, mode: RenderMode = renderMode) {
    def render() = {
      glDisable(GL_DEPTH_TEST)
      glDisable(GL_LIGHTING)
      
      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      
      glEnable(GL_TEXTURE_2D)
      glBindTexture(GL_TEXTURE_2D, texture)

      glPushMatrix()
        glTranslated(coord.x,coord.y,0)
        glColor4d(1,1,1,alpha)
        val (v0,v1) = if(flipy) (0f,1f) else (1f,0f)
        val (h0,h1) = if(flipx) (0f,1f) else (1f,0f)
        glBegin(GL_QUADS)
          glTexCoord2f(h1,v0); glVertex3d(0,       coord.h, 0)
          glTexCoord2f(h0,v0); glVertex3d(coord.w, coord.h, 0)
          glTexCoord2f(h0,v1); glVertex3d(coord.w, 0,       0)
          glTexCoord2f(h1,v1); glVertex3d(0,       0,       0)
        glEnd()
      glPopMatrix()

      glDisable(GL_TEXTURE_2D)

      glDisable(GL_BLEND)

      glEnable(GL_LIGHTING)
      glEnable(GL_DEPTH_TEST)
    }

    mode match {
      case Normal() => 
        cam.render
        render
      case Split() =>
        cam.render
        glEnable(GL_SCISSOR_TEST)
        glPushMatrix
          glScissor(0,0,winWidth/2, winHeight);
          glTranslated(-winWidth/4-eyeCorrection,0,0)
          render
        glPopMatrix
        glPushMatrix
          glScissor(winWidth/2,0,winWidth/2, winHeight);
          glTranslated(winWidth/4+eyeCorrection,0,0)
          render
        glPopMatrix
        glDisable(GL_SCISSOR_TEST)
    }
  }
}
