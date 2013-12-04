package org.ljudmila.liminoid

import org.lwjgl.opengl.GL11._
import math._
import Liminoid.{winWidth, winHeight, renderMode, RenderMode, Normal, Split, eyeCorrection}
import scala.collection.mutable

object OBJModel {
  val cache = mutable.HashMap[String, Model]()
  def apply(fileStr: String): Model = cache.getOrElseUpdate(fileStr, {
    val file = io.Source.fromFile(fileStr)

    val vReg = "v (.*?) (.*?) (.*?)".r
    val fReg = "f (.*)".r

    var vertices: Vertices = Vector()
    var faces: Faces = Vector()
    file.getLines foreach {
      case vReg(x,y,z) => vertices :+= Vec(x.toFloat, y.toFloat, z.toFloat)
      case fReg(f)     => faces :+= f.split(" ").map(_.toInt - 1)
      case _ => // nop
    }
    file.close

    Model(vertices, faces)
  })

  type Vertices = Vector[Vec]
  type Faces = Vector[Array[Int]]
  case class Vec(x: Double, y: Double, z: Double) {
    def +(v: Vec): Vec = Vec(x+v.x, y+v.y, z+v.z)
    def /=(f: Double): Vec = Vec(x/f, y/f, z/f)
    def minCoord(v: Vec): Vec = Vec(min(x,v.x), min(y,v.y), min(z,v.z))
    def maxCoord(v: Vec): Vec = Vec(max(x,v.x), max(y,v.y), max(z,v.z))
    def span(v: Vec): Vec = Vec(abs(x-v.x), abs(y-v.y), abs(z-v.z))
  }
  final val Vec0 = Vec(0,0,0)
  final val Vec1 = Vec(1,1,1)

  private val cam = new Camera
  cam.setViewPort(0,0,winWidth,winHeight)
  cam.setOrtho(0,winHeight,winWidth,0,1f,-1f)
  cam.setPerspective(90, (winWidth)/winHeight.toFloat, 1f, 1600f)
  cam.setPosition(0,0,-20);
  
  case class Model(vertices: Vertices, faces: Faces) {
    var cnt = 0

    lazy val (center, size) = {
      var (center, min, max) = (Vec0, Vec0, Vec0)
      for(v <- vertices) {
        center = center + v
        min = min minCoord v
        max = max maxCoord v
      }
      println((center /= vertices.size, min span max))
      (center /= vertices.size, min span max)
    }
    lazy val displayList = {
      val displayList = glGenLists(1)
      glNewList(displayList, GL_COMPILE)
      for(f <- faces) {
        glBegin(GL_POLYGON)
        for(i <- f) glVertex3d(vertices(i).x, vertices(i).y, vertices(i).z)
        glEnd()
      }
      glEndList()

      displayList
    }

    def render(mode: RenderMode = renderMode, p: Vec = Vec0, r: Vec = Vec0, s: Vec = Vec1) = {
      cnt += 1
      def render0() {
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)
        
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        
        glDisable(GL_TEXTURE_2D)
        //glBindTexture(GL_TEXTURE_2D, texture)

        glPushMatrix()
          glScaled(s.x, s.y, s.z)
          glTranslated(p.x, p.y, p.z)
          glRotated(r.x, 1,0,0)
          glRotated(r.y, 0,1,0)
          glRotated(r.z, 0,0,1)
          glTranslated(0, -center.y, 0)
          //glTranslated(-size.x/2, -size.y/2, -size.z/2)
          //glRotatef((cnt)%360,0,1,0)
          //glTranslated(+size.x/2, +size.y/2, +size.z/2)
          glTranslated(0, center.y, 0)
          glColor4d(0.1,0.1,0.1,0.9)
          glCallList(displayList)
        glPopMatrix()

        glDisable(GL_TEXTURE_2D)

        glDisable(GL_BLEND)

        glEnable(GL_LIGHTING)
        glEnable(GL_DEPTH_TEST)
      }

      mode match {
        case Normal() => 
          cam.render
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
