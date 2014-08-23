package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu.GLU
import math._
import Liminoid.{ winWidth, winHeight, renderMode, eyeCorrection, testNum1, osc1 }
import scala.collection.mutable
import java.io.File
import util.Random._
import scala.language.implicitConversions
import scala.annotation.switch
import Utils.{ TableRandom, pow2, getRatio }

sealed trait RenderMode
case object Mono extends RenderMode
case object Stereo extends RenderMode

final object GLAddons {
  def glMatrix(func: => Unit): Unit = {
    glPushMatrix
    func
    glPopMatrix
  }
  def glCapability(caps: Int*)(func: => Unit): Unit = {
    for(cap <- caps) glEnable(cap)
    func
    for(cap <- caps) glDisable(cap)
  }
  def glPrimitive(primitive: Int)(func: => Unit): Unit = {
    glBegin(primitive)
    func
    glEnd
  }
  def glDisplayList(toRender: => Unit): Int = {
    val displayList = glGenLists(1)
    glNewList(displayList, GL_COMPILE)
    toRender
    glEndList

    displayList
  }
  object gluQuadrics {
    import org.lwjgl.util.glu.{ Sphere, Cylinder, Disk, PartialDisk }
    lazy val smoothSphere = {
      val sphere = new Sphere
      sphere.setNormals(GLU.GLU_SMOOTH)
      sphere.setTextureFlag(false)
      
      sphere
    }
    lazy val texturedSphere = {
      val sphere = new Sphere
      sphere.setNormals(GLU.GLU_SMOOTH)
      sphere.setTextureFlag(true)
      
      sphere
    }
    
    lazy val sphere = new Sphere
    lazy val cylinder = new Cylinder
    lazy val disk = new Disk
    lazy val partialdisk = new PartialDisk
  }
}

final object Render {
  import GLAddons._
  
  lazy val cam = {
    val cam = new Camera
    cam.setViewPort(0,0, winWidth,winHeight)
    cam.setOrtho(0,winHeight,winWidth,0, 1f,-1f)
    cam.setPerspective(50, winWidth/winHeight.toFloat, 0.25f, 700f)
    cam.setPosition(0, 0, 0)
    cam.lookAt(Vec3(0, 0, 200))
    
    cam
  }

  // See dis? I need dis rendered
  def render3D(dis: => Unit): Unit = {
    renderMode match {
      case Mono =>
        dis
      case Stereo =>
        //TODO: name these
        val x = 0.5f       //+ Liminoid.testNum
        val x2 = 126+30+22 //+ Liminoid.testNum
        glCapability(GL_SCISSOR_TEST) {
          glMatrix {
            glScissor(0,0, winWidth/2,winHeight)
            cam.look(Vec3(-x, 0, 0), Vec3(-x2, 0, 0))
            dis
          }
          glMatrix {
            glScissor(winWidth/2,0, winWidth/2,winHeight)
            cam.look(Vec3(+x, 0, 0), Vec3(+x2, 0, 0))
            dis
          }
        }
    }
  }
  
  lazy val cam2D = {
    val cam2D = new Camera
    cam2D.setViewPort(0,0, winWidth,winHeight)
    cam2D.setOrtho(0,winHeight,winWidth,0, 1f,-1f)
    
    cam2D
  }
  def render2D(toRender: => Unit): Unit = {
    renderMode match {
      case Mono =>
        cam2D.render
        toRender
      case Stereo =>
        cam2D.render
        glCapability(GL_SCISSOR_TEST) {
          val eyeOffset = (winWidth/4+eyeCorrection)
          glScissor(0,0, winWidth/2,winHeight)
          glTranslatef(-eyeOffset, 0, 0)
          toRender

          glScissor(winWidth/2,0, winWidth/2,winHeight)
          glTranslatef(2*eyeOffset, 0, 0)
          toRender
          glTranslatef(-eyeOffset, 0, 0)
        }
    }
  }  
}
