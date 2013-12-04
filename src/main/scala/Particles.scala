package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import collection.mutable.ListBuffer
import util.Random._

case class Size(w: Float = 1, h: Float = 1)
case class Angle(roll: Float = 0, pitch: Float = 0, yaw: Float = 0)

case class Quad(coord: Vec3, size: Size = Size(), angle: Angle = Angle(), alpha: Float = 1) {
  def render() {
    glPushMatrix()
      glTranslated(coord.x,coord.y,coord.z)
      /*glTranslated(size.w/2,size.h/2,0)
      glRotatef(angle.roll,  1, 0, 0)
      glRotatef(angle.pitch, 0, 1, 0)
      glRotatef(angle.yaw,   0, 0, 1)
      glTranslated(-size.w/2,-size.h/2,0)*/
      glColor4d(1,1,1, alpha)
      glBegin(GL_QUADS)
        /*glTexCoord2f(1,0);*/ glVertex3d(-size.w/2, +size.h/2, 0)
        /*glTexCoord2f(0,0);*/ glVertex3d(+size.w/2, +size.h/2, 0)
        /*glTexCoord2f(0,1);*/ glVertex3d(+size.w/2, -size.h/2, 0)
        /*glTexCoord2f(1,1);*/ glVertex3d(-size.w/2, -size.h/2, 0)
      glEnd()
    glPopMatrix()
  }
}

case class Particle(var quad: Quad, deltaFunc: (Quad, Float) => Quad) {
  def step(delta: Float) {
    this.quad = deltaFunc(quad, delta)
  }
  def render() {
    quad.render()
  }
}

object Particles {
  import Liminoid.{winWidth, winHeight, renderMode, RenderMode, Normal, Split, eyeCorrection}

  private val cam = new Camera
  cam.setViewPort(0,0,winWidth,winHeight)
  //cam.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  //cam.setOrtho(0,winHeight,winWidth,0,100f,-100f)
  cam.setPerspective(90, (winWidth)/winHeight.toFloat, 1f, 1600f)
  
  val particles = ListBuffer[Particle]()
  def +=(p: Particle): Unit = { particles += p }

  var cnt = 0L

  def render(delta: Float, mode: RenderMode = renderMode) {
    cnt+=4
    def render0() = {
      glEnable(GL_DEPTH_TEST)
      //glEnable(GL_LIGHTING)
      
      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      
      glDisable(GL_TEXTURE_2D)
      //glEnable(GL_TEXTURE_2D)
      //glBindTexture(GL_TEXTURE_2D, texture)

      for(p <- particles) {
        //p.step(delta); p.render()
      }

      glRotatef((cnt)%360,4,2,1)
      glScalef(15,15,15)
      glBegin(GL_QUADS)
        // top
        glColor4d(0,0,1,1)
        glNormal3f( 0f, 1f, 0f)
        glVertex3f( 1f, 1f,-1f)
        glVertex3f(-1f, 1f,-1f)
        glVertex3f(-1f, 1f, 1f)
        glVertex3f( 1f, 1f, 1f)
        // bottom 
        glColor4d(0,1,0,1)
        glNormal3f( 0f,-1f, 1f)
        glVertex3f( 1f,-1f, 1f)
        glVertex3f(-1f,-1f, 1f)
        glVertex3f(-1f,-1f,-1f)
        glVertex3f( 1f,-1f,-1f)
        // Front
        glColor4d(1,0,0,1)
        glNormal3f( 0f, 0f, 1f)
        glVertex3f( 1f, 1f, 1f)
        glVertex3f(-1f, 1f, 1f) 
        glVertex3f(-1f,-1f, 1f)
        glVertex3f( 1f,-1f, 1f)
        // back
        glColor4d(1,0,1,1)
        glNormal3f( 0f, 0f,-1f)
        glVertex3f( 1f,-1f,-1f)
        glVertex3f(-1f,-1f,-1f)
        glVertex3f(-1f, 1f,-1f)
        glVertex3f( 1f, 1f,-1f)
        // left
        glColor4d(1,1,0,1)
        glNormal3f(-1f, 0f, 0f)
        glVertex3f(-1f, 1f, 1f)
        glVertex3f(-1f, 1f,-1f)
        glVertex3f(-1f,-1f,-1f)
        glVertex3f(-1f,-1f, 1f)
        // right
        glColor4d(0,1,1,1)
        glNormal3f( 1f, 0f, 0f)
        glVertex3f( 1f, 1f,-1f)
        glVertex3f( 1f, 1f, 1f)
        glVertex3f( 1f,-1f, 1f)
        glVertex3f( 1f,-1f,-1f)
      glEnd()

      glDisable(GL_TEXTURE_2D)
      glDisable(GL_BLEND)
    }

    mode match {
      /*case Normal() => 
        cam.setPosition(0, 0, -80)
        cam.render
        render0*/
      case _ =>
        //cam.setPosition(-1000,110,-80)
        //cam.lookAt(Vec3(0,0,0))
        cam.render
        glEnable(GL_SCISSOR_TEST)
        if(delta==1) {
        glPushMatrix
          //cam.setPosition(0, 0, -80)
          //cam.lookAt(Vec3(eyeCorrection,0,0))
          //cam.render
          glScissor(0,0,winWidth/2, winHeight);
          //glTranslated(-eyeCorrection/2,0,0)
          render0
        glPopMatrix} else {
        glPushMatrix
          //cam.setPosition(0, 0, -80)
          //cam.lookAt(Vec3(-eyeCorrection,0,0))
          //cam.render
          glScissor(winWidth/2,0,winWidth/2, winHeight);
          //glTranslated(+eyeCorrection/2,0,0)
          render0
        glPopMatrix}
        glDisable(GL_SCISSOR_TEST)
    }
  }
}
