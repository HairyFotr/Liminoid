package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{winWidth, winHeight, renderMode, RenderMode, Normal, Split, eyeCorrection}
import scala.collection.mutable

object OBJModel {
  val cache = mutable.HashMap[String, Model]()

  val oReg = "o (.*)".r              // object name, TODO: split models into objects if more than one
  val vReg = "v (.*?) (.*?) (.*?)".r //vertices
  val fReg = "f (.*)".r              //faces
  val fcReg = "(.*?)/(.*?)".r          //face coord
  
  val vtReg = "vt (.*?) (.*?)".r            //uv vertices

  def apply(fileStr: String): Model = cache.getOrElseUpdate(fileStr, {
    val file = io.Source.fromFile(fileStr)

    var vertices: Vertices = Vector()
    var faces: Faces = Vector()
    var uvVertices: UVVertices = Vector()
    //var name = ""
    file.getLines foreach {
      case vReg(x,y,z) => vertices :+= Vec(x.toDouble, y.toDouble, z.toDouble)
      case fReg(f)     => faces :+= f.split(" ").map { case fcReg(v, vt) => (v.toInt - 1, vt.toInt -1); case v => (v.toInt - 1, -1) }

      case vtReg(u,v)  => uvVertices :+= Vec2(u.toDouble, v.toDouble)

      //case oReg(n) => name = n
      case _ => // nop
    }
    file.close

    Model(vertices, faces, uvVertices)
  })

  type Vertices = Vector[Vec]
  type Faces = Vector[Array[(Int, Int)]]
  type UVVertices = Vector[Vec2]
  case class Vec(x: Double, y: Double, z: Double) {
    def +(v: Vec): Vec = Vec(x+v.x, y+v.y, z+v.z)
    def /(f: Double): Vec = Vec(x/f, y/f, z/f)
    def *(f: Double): Vec = Vec(x*f, y*f, z*f)
    def minCoord(v: Vec): Vec = Vec(min(x,v.x), min(y,v.y), min(z,v.z))
    def maxCoord(v: Vec): Vec = Vec(max(x,v.x), max(y,v.y), max(z,v.z))
    def span(v: Vec): Vec = Vec(abs(x-v.x), abs(y-v.y), abs(z-v.z))
  }
  final val Vec0 = Vec(0,0,0)
  final val Vec1 = Vec(1,1,1)

  case class Transform(var pos: Vec = Vec0, var rot: Vec = Vec0, var size: Vec = Vec1)
  final val Transform0 = Transform(Vec0, Vec0, Vec1)

  case class Vec2(u: Double, v: Double)

  //case class Color(r: Float, g: Float, b: Float, a: Float = 1)
  //final val white = Color(1,1,1)
  //final val black = Color(0,0,0)

  val cam = new Camera
  cam.setViewPort(0,0,winWidth,winHeight)
  cam.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  cam.setPerspective(90, (winWidth)/winHeight.toFloat, 1f, 1600f)
  cam.setPosition(0,0,-20);
  
  case class Model(vertices: Vertices, faces: Faces, uvVertices: UVVertices, name: String = "") {
    var cnt = 0

    /*lazy val (center, size) = {
      var (center, min, max) = (Vec0, Vec0, Vec0)
      for(v <- vertices) {
        center = center + v
        min = min minCoord v
        max = max maxCoord v
      }
      println((center /= vertices.size, min span max))
      (center /= vertices.size, min span max)
    }*/
    lazy val displayList = {
      val displayList = glGenLists(1)
      glNewList(displayList, GL_COMPILE)
      for(f <- faces) {
        glBegin(GL_POLYGON)
        for((vi, vti) <- f) {
          if(vti != -1) glTexCoord2d(uvVertices(vti).u, uvVertices(vti).v)
          glVertex3d(vertices(vi).x, vertices(vi).y, vertices(vi).z)
        }
        glEnd()
      }
      glEndList()

      displayList
    }

    def render(mode: RenderMode = renderMode, t: Transform = Transform0, tex: Int = -1, c: Vec = Vec1) = {
      cnt += 1
      def render0() {
        import t._
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)
        
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        
        if(tex != -1) {
          glEnable(GL_TEXTURE_2D)
          glBindTexture(GL_TEXTURE_2D, tex)
        } else {
          glDisable(GL_TEXTURE_2D)
        }

        glPushMatrix()
          glTranslated(pos.x, pos.y, pos.z)
          glRotated(rot.x, 1,0,0)
          glRotated(rot.y, 0,1,0)
          glRotated(rot.z, 0,0,1)
          //glTranslated(0, -center.y, 0)
          //glTranslated(-size.x/2, -size.y/2, -size.z/2)
          //glRotatef((cnt)%360,0,1,0)
          //glTranslated(+size.x/2, +size.y/2, +size.z/2)
          //glTranslated(0, center.y, 0)
          glScaled(size.x, size.y, size.z)
          glColor4d(c.x,c.y,c.z,1)
          glCallList(displayList)
        glPopMatrix()

        glDisable(GL_TEXTURE_2D)

        glDisable(GL_BLEND)

        glEnable(GL_LIGHTING)
        glEnable(GL_DEPTH_TEST)
      }

      mode match {
        case Normal() => 
          //cam.render
          render0
        case Split() =>
          val x = 15
          //cam.render
          glEnable(GL_SCISSOR_TEST)
          glPushMatrix
            glScissor(0,0,winWidth/2, winHeight);
            glTranslated(-winWidth/4-eyeCorrection,0,0)
            cam.pos.x -= x
            cam.lookAtV.x -= x
            cam.render
            cam.lookAtV.x += x
            cam.pos.x += x
            render0
          glPopMatrix
          glPushMatrix
            glScissor(winWidth/2,0,winWidth/2, winHeight);
            glTranslated(winWidth/4+eyeCorrection,0,0)
            cam.lookAtV.x += x
            cam.pos.x += x
            cam.render
            cam.pos.x -= x
            cam.lookAtV.x -= x
            render0
          glPopMatrix
          glDisable(GL_SCISSOR_TEST)
      }
    }
  }
}
