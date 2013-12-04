package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU

object gluQuadrics {
  import org.lwjgl.util.glu.{Sphere,Cylinder,Disk,PartialDisk}
  lazy val sphere = new Sphere
  lazy val cylinder = new Cylinder
  lazy val disk = new Disk
  lazy val partialdisk = new PartialDisk
}

abstract class Model {
  var pos = Vec3()
  var rot = Vec3()
  private var scal = Vec3(1f,1f,1f)
  def scale: Vec3 = scal
  def scale_=(v: Vec3) { scal = v }
  var visible = true

  def setPosition(x: Float, y: Float, z: Float) { pos = Vec3(x,y,z) }
  def setRotation(x: Float, y: Float, z: Float) { rot = Vec3(x,y,z) }
  def setScale(x: Float, y: Float, z: Float) { scale = Vec3(x,y,z) }
  def setPosition(v: Vec3) { pos = v.clone }
  def setRotation(v: Vec3) { rot = v.clone }
  def setScale(v: Vec3) { scale = v.clone }
  
  def doTranslate() {
    GL11.glTranslatef(pos.x, pos.y, pos.z)
  }
  def doRotate() {
    if(rot.z != 0) GL11.glRotatef(rot.z, 0, 0, 1)
    if(rot.y != 0) GL11.glRotatef(rot.y, 0, 1, 0)
    if(rot.x != 0) GL11.glRotatef(rot.x, 1, 0, 0)
  }
  def doScale() {
    GL11.glScalef(scale.x, scale.y, scale.z)
  }

  def doTransforms() {
    doTranslate()
    doRotate()
    doScale()
  }

  def render(): Unit

  override def toString: String = "p:("+pos.toString+"), " + "r:("+rot.toString+"), " + "s:("+scale.toString+")"
}

trait Cache { self: DisplayModel =>
  var compiled = false
  var displayList: Int = -1

  def compile() {
    displayList = GL11.glGenLists(1)
    GL11.glNewList(displayList, GL11.GL_COMPILE)
    renderfunc()
    GL11.glEndList()
    compiled = true
  }

  override def render() {
    if(visible) {
      GL11.glPushMatrix()
        doTransforms

        if(displayList == -1) {
          renderfunc()
        } else {
          GL11.glCallList(displayList)
        }
      GL11.glPopMatrix()
    }
  }
}

// doesn't care about points and stuff
class DisplayModel(var renderfunc: () => Unit = () => ()) extends Model {
  var (vector,vector2) = (Vec3(), Vec3())
/*  def reset(limit: Int = 1, preserveCurrent: Boolean = true) {
    if(compileCache.size > limit) {
      var count = 0
      compileCache.clone.foreach {
        case (id,listid) => 
          if(listid != displayList || !preserveCurrent) {
            count += 1
            Global.tasks = Global.tasks :+ (() => GL11.glDeleteLists(listid, 1))
            compileCache -= id
          }
      }
    }
  }
  def free() {
    reset(limit = 0, preserveCurrent = false)
    displayList = -1
  }*/

  override def clone: DisplayModel = {
    val res = new DisplayModel(this.renderfunc)
    res.pos = this.pos.clone
    res.rot = this.rot.clone
    res.scale = this.scale.clone
    res
  }
  
  override def render() {
    if(visible) {
      GL11.glPushMatrix()
        doTransforms
        renderfunc()
      GL11.glPopMatrix()
    }
  }
}

class GeneratorModel(generator: () => Object, draw: Object => Unit) extends DisplayModel {
  var data: Object = generator()
  var box: BoundingBox = new BoundingBox(Vec3())
  renderfunc = () => { draw(data); () }
  //idfunc = _idfunc
  
  def regenerate() {
    data = generator()
    //compile()
  }
  
  // make a data constructor, so clone has same data. (eliminate generator in static constructor)
  override def clone: GeneratorModel = {
    val res = new GeneratorModel(generator, draw)
    res.pos = this.pos.clone
    res.rot = this.rot.clone
    res.scale = this.scale.clone
    res
  }
}

class Camera extends Model {
  // default projection 
  var perspective = false
  var (near,far) = (1f,30f) // near, far clipping plane
  var (fov,aspectRatio) = (45f,4/3f) // perspective stuff
  var (minX,minY,maxX,maxY) = (-1f,-1f, 1f, 1f) // ortho stuff
  var projectionChanged = true // do we need to remake projection matrix
  var vector = Vec3()
  var angle = Vec3()
  var viewPort = (0,0,0,0)

  def setViewPort(x: Int, y: Int, xx: Int, yy: Int) {
    GL11.glViewport(x,y,xx,yy)
    viewPort = (x,y,xx,yy)
  }

  // set a perspective projection
  def setPerspective(fv: Float, ar: Float, n: Float, f: Float) {
    perspective = true
    fov = fv
    aspectRatio = ar
    near = n
    far = f
    projectionChanged = true
  }
  
  // set an ortographic projection
  def setOrtho(mx: Float, my: Float, Mx: Float, My: Float, n: Float, f: Float) {
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
  def lookAt(m: Model): Unit = lookAtV = m.pos.clone
    
  override def render() {
    // setup projection matrix stack
    //if(projectionChanged) {
      projectionChanged = false
      GL11.glFlush()
      GL11.glViewport(viewPort._1,viewPort._2,viewPort._3,viewPort._4)
      GL11.glMatrixMode(GL11.GL_PROJECTION)
      GL11.glLoadIdentity()
      if(perspective) {
        // perspective projection
        GLU.gluPerspective(fov,aspectRatio, near,far)
      } else {
        // orthographic projection 
        GL11.glOrtho(minX,maxX, minY,maxY, near,far)
      }
    //}

    // model view stack 
    GL11.glMatrixMode(GL11.GL_MODELVIEW)
    GL11.glLoadIdentity()
    if(perspective) {
      GLU.gluLookAt(pos.x,pos.y,pos.z,       // camera position
                    lookAtV.x,lookAtV.y,lookAtV.z, // look-at vector
                    0,1,0)             // up vector 
    }
  }
}

class ModelLink(m1: Model, m2: Model, var vector: Vec3=Vec3(), var vector2: Vec3=Vec3()) {
  private var linked = false
  def isLinked: Boolean = linked
  def breakLink() { linked = false }
  def forgeLink() { linked = true }
  
  def applyLink() {
    if(linked) {
      m1.setPosition(m2.pos+vector)
      m1.setRotation(m2.rot+vector2)
    }
  }
}
