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

  sealed trait OldModel {
    var pos = Vec3()
    var rot = Vec3()
    private var scal = Vec3(1f, 1f, 1f)
    def scale: Vec3 = scal
    def scale_=(v: Vec3): Unit = { scal = v }
    var visible = true
  
    def setPosition(x: Float, y: Float, z: Float): Unit = { pos = Vec3(x, y, z) }
    def setRotation(x: Float, y: Float, z: Float): Unit = { rot = Vec3(x, y, z) }
    def setScale(x: Float, y: Float, z: Float): Unit = { scale = Vec3(x, y, z) }
    def setPosition(v: Vec3): Unit = { pos = v.copy }
    def setRotation(v: Vec3): Unit = { rot = v.copy }
    def setScale(v: Vec3): Unit = { scale = v.copy }
    
    def doTranslate(): Unit = {
      glTranslatef(pos.x, pos.y, pos.z)
    }
    def doRotate(): Unit = {
      if(rot.z != 0) glRotatef(rot.z, 0, 0, 1)
      if(rot.y != 0) glRotatef(rot.y, 0, 1, 0)
      if(rot.x != 0) glRotatef(rot.x, 1, 0, 0)
    }
    def doScale(): Unit = {
      glScalef(scale.x, scale.y, scale.z)
    }
  
    def doTransforms(): Unit = {
      doTranslate()
      doRotate()
      doScale()
    }
  
    def render(): Unit
  
    override def toString: String = "p:("+pos.toString+"), " + "r:("+rot.toString+"), " + "s:("+scale.toString+")"
  }
  
  final class Camera extends OldModel {
    // default projection
    var perspective = false
    var (near, far) = (1f, 30f) // near, far clipping plane
    var (fov, aspectRatio) = (45f, 4f/3f) // perspective stuff //TODO: is 4/3 right?
    var (minX, minY, maxX, maxY) = (-1d, -1d, 1d, 1d) // ortho stuff
    var projectionChanged = true // do we need to remake projection matrix
    var vector = Vec3()
    var angle = Vec3()
    var viewPort = (0, 0, 0, 0)
  
    def setViewPort(x: Int, y: Int, xx: Int, yy: Int): Unit = {
      glViewport(x,y, xx,yy)
      viewPort = (x,y, xx,yy)
    }
  
    // set a perspective projection
    def setPerspective(fv: Float, ar: Float, n: Float, f: Float): Unit = {
      perspective = true
      fov = fv
      aspectRatio = ar
      near = n
      far = f
      projectionChanged = true
    }
    
    // set an ortographic projection
    def setOrtho(mx: Double, my: Double, Mx: Double, My: Double, n: Float, f: Float): Unit = {
      perspective = false
      minX = mx
      minY = my
      maxX = Mx
      maxY = My
      near = n
      far = f
      projectionChanged = true
    }
    
    var lookAtV = Vec3()
    def lookAt(v: Vec3): Unit = lookAtV = v.copy
    def lookAt(m: OldModel): Unit = lookAtV = m.pos.copy
      
    override def render(): Unit = {
      // setup projection matrix stack
      //if(projectionChanged) {
        projectionChanged = false
        glFlush()
        glViewport(viewPort._1, viewPort._2, viewPort._3, viewPort._4)
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        if(perspective) {
          // perspective projection
          GLU.gluPerspective(fov, aspectRatio, near,far)
        } else {
          // orthographic projection
          glOrtho(minX,maxX, minY,maxY, near,far)
        }
      //}
  
      // model view stack
      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()
      if(perspective) {
        GLU.gluLookAt(pos.x, pos.y, pos.z,             // camera position
                      lookAtV.x, lookAtV.y, lookAtV.z, // look-at vector
                      0, 1, 0)                         // up vector
      }
    }
    def look(v: Vec3, a: Vec3): Unit = {
      glLoadIdentity()
      val aa = lookAtV + a
      GLU.gluLookAt(v.x, v.y, v.z,    // camera position
                    aa.x, aa.y, aa.z, // look-at vector
                    0, 1, 0)          // up vector
      glRotatef(rot.x, 1, 0, 0)
      glRotatef(rot.y, 0, 1, 0)
      glRotatef(rot.z, 0, 0, 1)
    }
  }
}
