package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU

object gluQuadrics {
  import org.lwjgl.util.glu.{ Sphere, Cylinder, Disk, PartialDisk }
  lazy val sphereTex = {
    val sphere = new Sphere
    sphere.setNormals(GLU.GLU_SMOOTH)
    sphere.setTextureFlag(true)
    
    sphere
  }
  lazy val sphere = {
    val sphere = new Sphere
    sphere.setNormals(GLU.GLU_SMOOTH)
    sphere.setTextureFlag(false)
    
    sphere
  }
  lazy val cylinder = new Cylinder
  lazy val disk = new Disk
  lazy val partialdisk = new PartialDisk
}

// Deprecated, see Model.scala

trait OldModel {
  var pos = Vec3()
  var rot = Vec3()
  private var scal = Vec3(1f, 1f, 1f)
  def scale: Vec3 = scal
  def scale_=(v: Vec3): Unit = { scal = v }
  var visible = true

  def setPosition(x: Float, y: Float, z: Float): Unit = { pos = Vec3(x, y, z) }
  def setRotation(x: Float, y: Float, z: Float): Unit = { rot = Vec3(x, y, z) }
  def setScale(x: Float, y: Float, z: Float): Unit = { scale = Vec3(x, y, z) }
  def setPosition(v: Vec3): Unit = { pos = v.clone }
  def setRotation(v: Vec3): Unit = { rot = v.clone }
  def setScale(v: Vec3): Unit = { scale = v.clone }
  
  def doTranslate(): Unit = {
    GL11.glTranslatef(pos.x, pos.y, pos.z)
  }
  def doRotate(): Unit = {
    if(rot.z != 0) GL11.glRotatef(rot.z, 0, 0, 1)
    if(rot.y != 0) GL11.glRotatef(rot.y, 0, 1, 0)
    if(rot.x != 0) GL11.glRotatef(rot.x, 1, 0, 0)
  }
  def doScale(): Unit = {
    GL11.glScalef(scale.x, scale.y, scale.z)
  }

  def doTransforms(): Unit = {
    doTranslate()
    doRotate()
    doScale()
  }

  def render(): Unit

  override def toString: String = "p:("+pos.toString+"), " + "r:("+rot.toString+"), " + "s:("+scale.toString+")"
}

class Camera extends OldModel {
  // default projection
  var perspective = false
  var (near, far) = (1f, 30f) // near, far clipping plane
  var (fov, aspectRatio) = (45f, 4/3f) // perspective stuff
  var (minX, minY, maxX, maxY) = (-1f, -1f, 1f, 1f) // ortho stuff
  var projectionChanged = true // do we need to remake projection matrix
  var vector = Vec3()
  var angle = Vec3()
  var viewPort = (0, 0, 0, 0)

  def setViewPort(x: Int, y: Int, xx: Int, yy: Int): Unit = {
    GL11.glViewport(x,y, xx,yy)
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
  def setOrtho(mx: Float, my: Float, Mx: Float, My: Float, n: Float, f: Float): Unit = {
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
  def lookAt(v: Vec3): Unit = lookAtV = v.clone
  def lookAt(m: OldModel): Unit = lookAtV = m.pos.clone
    
  override def render(): Unit = {
    // setup projection matrix stack
    //if(projectionChanged) {
      projectionChanged = false
      GL11.glFlush()
      GL11.glViewport(viewPort._1, viewPort._2, viewPort._3, viewPort._4)
      GL11.glMatrixMode(GL11.GL_PROJECTION)
      GL11.glLoadIdentity()
      if(perspective) {
        // perspective projection
        GLU.gluPerspective(fov, aspectRatio, near,far)
      } else {
        // orthographic projection
        GL11.glOrtho(minX,maxX, minY,maxY, near,far)
      }
    //}

    // model view stack
    GL11.glMatrixMode(GL11.GL_MODELVIEW)
    GL11.glLoadIdentity()
    if(perspective) {
      GLU.gluLookAt(pos.x, pos.y, pos.z,             // camera position
                    lookAtV.x, lookAtV.y, lookAtV.z, // look-at vector
                    0, 1, 0)                         // up vector
    }
  }
  def look(v: Vec3, a: Vec3): Unit = {
    GL11.glLoadIdentity()
    val aa = lookAtV + a
    GLU.gluLookAt(v.x, v.y, v.z,    // camera position
                  aa.x, aa.y, aa.z, // look-at vector
                  0, 1, 0)          // up vector
    GL11.glRotated(rot.x, 1, 0, 0)
    GL11.glRotated(rot.y, 0, 1, 0)
    GL11.glRotated(rot.z, 0, 0, 1)
  }
}
